{-
Identity monad
==============
-}

import Control.Monad (ap, liftM)
import Data.Function ((&))

newtype Identity a = Identity {runIdentity :: a}
  deriving (Eq, Show)

{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
-}

-- NOTE: from GHCi 7.10, we have to implement Applicative for our type
--       based on https://stepik.org/lesson/8437/step/8?discussion=117854&reply=117875&unit=1572
instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure = return
  (<*>) = ap

--

instance Monad Identity where
  return = Identity
  Identity x >>= k = k x

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

-- >>> runIdentity (wrap'n'succ 3)
-- >>> wrap'n'succ 3
-- >>> runIdentity $ wrap'n'succ 3
-- 4
-- Identity {runIdentity = 4}
-- 4

-- >>> runIdentity $ wrap'n'succ 3 >>= wrap'n'succ
-- >>> runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
-- 5
-- 6

-- >>> 3 & succ & succ & succ
-- >>> succ 3 & succ & succ
-- 6
-- 6

-- >>> runIdentity $ return 3 >>= wrap'n'succ >>= wrap'n'succ >>= wrap'n'succ
-- 6

{-
TASK
====
Если некоторый тип является представителем класса `Monad`, то его можно сделать
представителем класса `Functor`, используя функцию `return` и оператор `>>=`.
Причём, это можно сделать даже не зная, как данный тип устроен.

Пусть вам дан тип

```
data SomeType a = ...
```

и он является представителем класса `Monad`. Сделайте его представителем класса
`Functor`.

SOLUTION
========
-}
-- data SomeType a = SomeType a

-- instance Functor SomeType where
--   fmap f x = x >>= k
--     where
--       k x = return (f x)

--------------------------------------------------------------------------------

{-
1st monad law
-------------
return a >>= k === k a
-}

-- >>> runIdentity $ wrap'n'succ 3
-- 4

-- >>> runIdentity $ wrap'n'succ 3 >>= return
-- 4

{-
2nd monad law
-------------
m >>= return === m
-}

{-
3rd monad law
-------------
m >>= k >>= k' === m >>= (\x -> k x >>= k')
-}

-- >>> runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
-- >>> runIdentity $ (wrap'n'succ 3 >>= wrap'n'succ) >>= wrap'n'succ
-- >>> runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= wrap'n'succ)
-- >>> runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= \y -> wrap'n'succ y)
-- 6
-- 6
-- 6
-- 6

{-
TASK
====
Вспомним тип `Log`

```
data Log a = Log [String] a
```

который мы сделали монадой в предыдущем модуле. Функция `return` для `Log`
оборачивает переданное значение в лог с пустым списком сообщений. Оператор `>>=`
возвращает лог с модифицированным значением и новым списком сообщений, который
состоит из прежнего списка и добавленного в конец списка сообщений, полученных
при модификации значения.

Пусть теперь функция `return` будет оборачивать переданное значение в список,
содержащий одно стандартное сообщение `"Log start"`.

Выберите верные утверждения относительно выполнения законов для монады с новым
поведением функции return.

SOLUTION
========
[x] Не выполняется первый закон
[x] Не выполняется второй закон
[ ] Не выполняется третий закон
[ ] Все законы выполняются
-}

--------------------------------------------------------------------------------

{-
TASK
====
Продолжим обсуждать монаду для `Log`. Пусть теперь у нас будет новая версия
оператора `>>=`, которая будет добавлять сообщения не в конец результирующего
списка, а в начало (при этом функция `return` предполагается возвращенной к
исходной реализации).

Выберите верные утверждения относительно выполнения законов для монады с новым
поведением оператора `>>=`.

SOLUTION
========
[ ] Не выполняется первый закон
[ ] Не выполняется второй закон
[ ] Не выполняется третий закон
[x] Все законы выполняются
-}

--------------------------------------------------------------------------------

{-
TASK
====
И снова монада `Log`. Пусть теперь оператор `>>=` будет добавлять сообщения как
в начало списка, так и в конец.

Выберите верные утверждения относительно выполнения законов для монады с новым
поведением оператора `>>=`.

SOLUTION
========
[x] Не выполняется 1-й закон
[ ] Не выполняется 2-й закон
[x] Не выполняется 3-й закон
[ ] Все законы выполняются
-}

--------------------------------------------------------------------------------

-- >>> runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
-- 6

goWrap0 =
  wrap'n'succ 3
    >>= wrap'n'succ
    >>= wrap'n'succ
    >>= return

-- >>> runIdentity goWrap0
-- 6

goWrap1 =
  wrap'n'succ 3
    >>= ( \x ->
            wrap'n'succ x
              >>= ( \y ->
                      wrap'n'succ y
                        >>= ( \z ->
                                return z
                            )
                  )
        )

-- >>> runIdentity goWrap1
-- 6

{-
NOTE: to disable formatting:

                                      -- Like imperative code:
goWrap2 = wrap'n'succ 3 >>= ( \x ->   -- x := succ 3;
          wrap'n'succ x >>= ( \y ->   -- y := succ x;
          wrap'n'succ y >>= ( \z ->   -- z := succ y;
          return (x, y, z))))         -- return (x, y, z)
-}

goWrap2 =
  wrap'n'succ 3
    >>= ( \x -> --
            wrap'n'succ x
              >>= ( \y -> --
                      wrap'n'succ y
                        >>= ( \z -> --
                                return (x, y, z) --
                            )
                  )
        )

-- >>> runIdentity goWrap2
-- (4,5,6)

goWrap3 =
  wrap'n'succ 3
    >>= ( \x -> --
            wrap'n'succ x
              >>= ( \y -> --
                      wrap'n'succ y >> return (x + y)
                  )
        )

-- >>> goWrap3
-- Identity {runIdentity = 9}

{-
Do notation
-----------
do { e1; e2 } === e1 >> e2
do { p <- e1; e2 } === e1 >>= \p -> e2
do { let v = e1; e2 } === let v = e1 in do e2
-}

goWrap4 =
  let i = 3
   in wrap'n'succ i
        >>= ( \x -> --
                wrap'n'succ x
                  >>= ( \y -> --
                          wrap'n'succ y >> return (i, x + y)
                      )
            )

-- >>> runIdentity goWrap4
-- (3,9)

goWrap5 = do
  let i = 3
  x <- wrap'n'succ i
  y <- wrap'n'succ x
  wrap'n'succ y
  return (i, x + y)

-- >>> runIdentity goWrap5
-- (3,9)

-- >>> :t goWrap5
-- goWrap5 :: Identity (Integer, Integer)
