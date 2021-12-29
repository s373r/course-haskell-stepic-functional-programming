import Control.Monad (ap, liftM)
import Data.Function ((&))
import Prelude hiding ((<=<), (=<<))

{-
Monad definition
================
-}

{-
Pure function
-------------
f :: a -> b

Side effects
------------
f :: a -> Maybe b
f :: a -> [b]
f :: a -> (Either s) b
f :: a -> (s, b)
f :: a -> ((->) e) b
f :: a -> (State s) b
f :: a -> IO b
-------------
f :: a -> m b - Kleisli arrow
-}

{-
TASK
====
Введём следующий тип:

```
data Log a = Log [String] a
```

Реализуйте вычисление с логированием, используя `Log`. Для начала определите
функцию `toLogger`

```
toLogger :: (a -> b) -> String -> (a -> Log b)
```

которая превращает обычную функцию, в функцию с логированием:

```
GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4
```

```
GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6
```
Далее, определите функцию `execLoggers`

```
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
```

Которая принимает некоторый элемент и две функции с логированием. `execLoggers`
возвращает результат последовательного применения функций к элементу и список
сообщений, которые были выданы при применении каждой из функций:

```
GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
```

SOLUTION
========
-}
data Log a = Log [String] a
  -- NOTE: added for unit-tests
  deriving (Eq, Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a f g = Log (fm ++ gm) c
  where
    (Log fm b) = f a
    (Log gm c) = g b

--

add1Log :: Int -> Log Int
add1Log = toLogger (+ 1) "added one"

mult2Log :: Int -> Log Int
mult2Log = toLogger (* 2) "multiplied by 2"

-- >>> add1Log 3 == Log ["added one"] 4
-- >>> mult2Log 3 == Log ["multiplied by 2"] 6
-- >>> execLoggers 3 add1Log mult2Log == Log ["added one","multiplied by 2"] 8
-- True
-- True
-- True

--------------------------------------------------------------------------------

{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b -- named as [monadic] bind

infixl 1 >>=
-}

-- >>> :t return
-- return :: Monad m => a -> m a

-- >>> :t return True
-- return True :: Monad m => m Bool

-- >>> return True :: [] Bool
-- [True]

-- >>> return True :: Maybe Bool
-- Just True

toKleisli :: Monad m => (a -> b) -> (a -> m b)
-- toKleisli f = \x -> return (f x)
toKleisli f = return . f

-- OR
toKleisli' :: Monad m => (a -> b) -> a -> m b
toKleisli' f x = return (f x)

-- >>> :t toKleisli cos
-- toKleisli cos :: (Monad m, Floating b) => b -> m b

-- >>> toKleisli cos 0 :: [Double]
-- >>> toKleisli cos 0 :: Maybe Double
-- >>> toKleisli cos 0 :: IO Double
-- [1.0]
-- Just 1.0
-- 1.0

{-
TASK
====
Функции с логированием из предыдущего задания возвращают в качестве результата
значение с некоторой дополнительной информацией в виде списка сообщений. Этот
список является контекстом. Реализуйте функцию `returnLog`

```
returnLog :: a -> Log a
```

которая является аналогом функции `return` для контекста `Log`. Данная функция
должна возвращать переданное ей значение с пустым контекстом.

SOLUTION
========
-}
returnLog :: a -> Log a
returnLog = Log []

--------------------------------------------------------------------------------

{-
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
-}

--     <-----------------
-- >>> (+1) $ (*3) $ (+2) $ 5
-- 22

--         ----------------->
-- >>> 5 & (+2) & (*3) & (+1)
-- 22

-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- >>> :t flip fmap
-- flip fmap :: Functor f => f a -> (a -> b) -> f b

{-
TASK
====
Реализуйте фукцию `bindLog`

```
bindLog :: Log a -> (a -> Log b) -> Log b
```

которая работает подобно оператору `>>=` для контекста `Log`.

```
GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1
```

```
GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
```

SOLUTION
========
-}
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log e a) f = Log (e ++ e') b
  where
    (Log e' b) = f a

-- >>> Log ["nothing done yet"] 0 `bindLog` add1Log == Log ["nothing done yet","added one"] 1
-- >>> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log == Log ["nothing done yet","added one","multiplied by 2"] 8
-- True
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Реализованные ранее `returnLog` и `bindLog` позволяют объявить тип `Log`
представителем класса `Monad`:

```
instance Monad Log where
    return = returnLog
    (>>=) = bindLog
```

Используя `return` и `>>=`, определите функцию `execLoggersList`

```
execLoggersList :: a -> [a -> Log a] -> Log a
```

которая принимает некоторый элемент, список функций с логированием и возвращает
результат последовательного применения всех функций в списке к переданному
элементу вместе со списком сообщений, которые возвращались данными функциями:

```
GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
```

SOLUTION
========
-}

-- NOTE: from GHCi 7.10, we have to implement Applicative for our type
--       based on https://stepik.org/lesson/8437/step/8?discussion=117854&reply=117875&unit=1572
instance Monad Log where
  return = returnLog
  (>>=) = bindLog

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

--

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl (>>=) (return a)

-- >>> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)] == Log ["added one","multiplied by 2","multiplied by 100"] 800
-- True

--------------------------------------------------------------------------------

{-
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail s = error s
-}

-- NOTE: similar to `$`
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- NOTE: left fish operator :)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \x -> g x >>= f

-- (<=<) f g x = g x >>= f

-- >>> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
