{-
State monad
===========
-}

import Control.Monad
  ( replicateM,
    replicateM_,
  )
import Control.Monad.Reader
  ( Reader,
    ask,
    asks,
    runReader,
  )
import Control.Monad.State
  ( State,
    evalState,
    execState,
    get,
    modify,
    put,
    runState,
    state,
  )
import Control.Monad.Writer
  ( Writer,
    execWriter,
    runWriter,
    tell,
  )

{-
newtype State s a = State {runState :: s -> (s, a)}

instance Monad (State s) where
  runState :: State s a -> s -> (a, s)
  return a = State $ \st -> (a, st)

  m >>= k = State $ \s ->
    let (a, st') = runState m st
        m' = k a
     in runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> s
evalState m s = fst (runState m s)
-}

{-
TASK
====
Выберите все верные утверждения про монаду `State`:

[x] Монада `Writer` является частным случаем монады `State`
[ ] Монада `State` является частным случаем монады `Writer`
[x] Монада `Reader` является частным случаем монады `State`

SOLUTION
========
-}

--------------------------------------------------------------------------------

{-
TASK
====
Где реализована монада `State`?

SOLUTION
========
( ) Монада State встроена в компилятор GHC, поскольку позволяет осуществлять
    вычисления с изменяемым состоянием, что невозможно в «чистом» Хаскеле
(x) Монада State реализована в одном из пакетов Haskell Platform
-}

--------------------------------------------------------------------------------

{-
get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put st = State $ \_ -> ((), st)
-}

-- >>> runState get 5
-- (5,5)

-- >>> runState (put 7) 5
-- ((),7)

tick :: State Int Int
tick = do
  n <- get
  put (n + 1)
  return n

-- >>> runState tick 5
-- (5,6)

{-
modify :: (s -> s) -> State s ()
modify f = do -- or: State $ \s -> ((), f s)
  s <- get
  put (f s)
-}

-- >>> runState (modify (^2)) 5
-- ((),25)

{-
TASK
====
Давайте убедимся, что с помощью монады `State` можно эмулировать монаду
`Reader`.

Напишите функцию `readerToState`, «поднимающую» вычисление из монады `Reader`
в монаду `State`:

```
GHCi> evalState (readerToState $ asks (+2)) 4
6
GHCi> runState (readerToState $ asks (+2)) 4
(6,4)
```

SOLUTION
========
-}

readerToState :: Reader r a -> State r a
readerToState m = state (\s -> (runReader m s, s))

-- >>> evalState (readerToState $ asks (+2)) 4 == 6
-- >>> runState (readerToState $ asks (+2)) 4 == (6,4)
-- True
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Теперь убедимся, что с помощью монады `State` можно эмулировать монаду `Writer`.

Напишите функцию `writerToState`, «поднимающую» вычисление из монады `Writer`
в монаду `State`:

```
GHCi> runState (writerToState $ tell "world") "hello,"
((),"hello,world")
GHCi> runState (writerToState $ tell "world") mempty
((),"world")
```

Обратите внимание на то, что при работе с монадой `Writer` предполагается,
что изначально лог пуст (точнее, что в нём лежит нейтральный элемент моноида),
поскольку интерфейс монады просто не позволяет задать стартовое значение.
Монада `State` же начальное состояние (оно же стартовое значение в логе)
задать позволяет.

SOLUTION
========
-}

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = state f
  where
    f s = (a, s `mappend` w)
      where
        (a, w) = runWriter m

-- >>> runState (writerToState $ tell "world") "hello," == ((),"hello,world")
-- >>> runState (writerToState $ tell "world") mempty == ((),"world")
-- True
-- True

--------------------------------------------------------------------------------

succ' :: Int -> Int
succ' n = execState tick n

-- >>> succ' 3
-- 4

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x

-- >>> 4 `plus` 5
-- 9

{-
replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n
-}

plus' :: Int -> Int -> Int
plus' n x = execState (replicateM n tick) x

-- >>> 4 `plus'` 5
-- 9

{-
TASK
====
Если бы мы хотели вычислить n-е число Фибоначчи на императивном языке
программирования, мы бы делали это с помощью двух переменных и цикла,
обновляющего эти переменные:

```python
def fib(n):
  a, b = 0, 1
  for i in [1 .. n]:
    a, b = b, a + b
  return a
```

С точки зрения Хаскеля, такую конструкцию удобно представлять себе
как вычисление с состоянием. Состояние в данном случае — это
два целочисленных значения.

Императивный алгоритм действует очень просто: он совершает `n` шагов,
каждый из которых некоторым образом изменяет текущее состояние. Первым делом,
реализуйте функцию `fibStep`, изменяющую состояние таким же образом,
как и один шаг цикла в императивном алгоритме:

```
GHCi> execState fibStep (0,1)
(1,1)
GHCi> execState fibStep (1,1)
(1,2)
GHCi> execState fibStep (1,2)
(2,3)
```

После этого останется лишь применить этот шаг n раз к правильному стартовому
состоянию и выдать ответ. Реализуйте вспомогательную функцию `execStateN`,
которая принимает число шагов nnn, вычисление с состоянием и начальное
состояние, запускает вычисление nnn раз и выдает получившееся состояние
(игнорируя сами результаты вычислений). Применяя эту функцию к `fibStep`,
мы сможем вычислять числа Фибоначчи:

```
fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
```

SOLUTION
========
-}

fibStep :: State (Integer, Integer) ()
fibStep = state $ \(a, b) -> ((), (b, a + b))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM_ n m)

-- >>> execState fibStep (0,1) == (1,1)
-- >>> execState fibStep (1,1) == (1,2)
-- >>> execState fibStep (1,2) == (2,3)
-- True
-- True
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Некоторое время назад мы определили тип двоичных деревьев, содержащих
значения в узлах:

```
data Tree a = Leaf a | Fork (Tree a) a (Tree a)
```

В этой задаче вам дано значение типа `Tree ()`, иными словами, вам задана
форма дерева. Требуется пронумеровать вершины дерева данной формы,
обойдя их in-order (то есть, сначала обходим левое поддерево,
затем текущую вершину, затем правое поддерево):

```
GHCi> numberTree (Leaf ())
Leaf 1
GHCi> numberTree (Fork (Leaf ()) () (Leaf ()))
Fork (Leaf 1) 2 (Leaf 3)
```

SOLUTION
========
-}
data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving (Eq, Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (go tree) 1
  where
    go (Leaf _) = do
      n <- postIncrement
      return $ Leaf n
    go (Fork l _ r) = do
      l' <- go l
      v' <- postIncrement
      r' <- go r
      return $ Fork l' v' r'
    postIncrement = do
      x <- get
      put (x + 1)
      return x

-- >>> numberTree (Leaf ()) == Leaf 1
-- >>> numberTree (Fork (Leaf ()) () (Leaf ())) == Fork (Leaf 1) 2 (Leaf 3)
-- True
-- True

--------------------------------------------------------------------------------
