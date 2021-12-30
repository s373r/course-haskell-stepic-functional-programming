{-
List and Maybe as monads
========================
-}
import Control.Monad (ap, guard, liftM)
import qualified Control.Monad.Fail as Fail
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- import Prelude hiding (Just, Maybe, Nothing)

-- data Maybe a = Nothing | Just a
--   deriving (Eq, Ord)

{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  fail :: String -> m a
-}

-- NOTE: from GHCi 7.10, we have to implement Applicative for our type
--       based on https://stepik.org/lesson/8437/step/8?discussion=117854&reply=117875&unit=1572
-- instance Functor Maybe where
--   fmap = liftM

-- instance Applicative Maybe where
--   pure = return
--   (<*>) = ap

--

-- instance Monad Maybe where
--   return = Just

--   (Just x) >>= k = k x
--   Nothing >>= _ = Nothing

--   (Just _) >> m = m
--   Nothing >> _ = Nothing

-- instance Fail.MonadFail Maybe where
--   fail _ = Nothing

--------------------------------------------------------------------------------

type Name = String

--          child name, parent name
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers =
  [ ("Bill", "John"),
    ("Ann", "John"),
    ("John", "Piter")
  ]
mothers =
  [ ("Bill", "Jane"),
    ("Ann", "Jane"),
    ("John", "Alice"),
    ("Jane", "Dorothy"),
    ("Alice", "Mary")
  ]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

-- >>> :t lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- NOTE: get great-grandmother
-- >>> getF "Bill" >>= getM >>= getM
-- >>> do {f <- getF "Bill"; gm <- getM f; getM gm}
-- Just "Mary"
-- Just "Mary"

granmas :: Name -> Maybe (Name, Name)
granmas person = do
  m <- getM person
  gmm <- getM m
  f <- getF person
  gmf <- getF f
  return (gmm, gmf)

-- >>> granmas "Ann"
-- Just ("Dorothy","Piter")

-- >>> granmas "Jonh"
-- Nothing

{-
TASK
====
Рассмотрим язык арифметических выражений, которые состоят из чисел, скобок,
операций сложения и вычитания. Конструкции данного языка можно представить
следующим типом данных:

```
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)
```

Реализуйте лексер арифметических выражений. Для начала реализуйте следующую
функцию:

```
asToken :: String -> Maybe Token
```

Она проверяет, является ли переданная строка числом (используйте функцию
`isDigit` из модуля `Data.Char`), знаком `"+"` или `"-"`, открывающейся или
закрывающейся скобкой. Если является, то она возвращает нужное значение
обёрнутое в `Just`, в противном случае - `Nothing`:

```
GHCi> asToken "123"
Just (Number 123)
```

```
GHCi> asToken "abc"
Nothing
```

Далее, реализуйте функцию `tokenize`:

```
tokenize :: String -> Maybe [Token]
```

Функция принимает на вход строку и если каждое слово является корректным
токеном, то она возвращает список этих токенов, завёрнутый в `Just`. В противном
случае возвращается `Nothing`.

Функция должна разбивать входную строку на отдельные слова по пробелам
(используйте библиотечную функцию `words`). Далее, полученный список строк
должен быть свёрнут с использованием функции `asToken` и свойств монады `Maybe`:

```
GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]
```

```
GHCi> tokenize "1 + ( 7 - 2 )"
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
```

```
GHCi> tokenize "1 + abc"
Nothing
```

Обратите внимание, что скобки отделяются пробелами от остальных выражений!

SOLUTION
========
-}

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

-- NOTE: imported above
-- import Text.Read (readMaybe)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken value = do
  number <- readMaybe value
  return (Number number)

tokenize :: String -> Maybe [Token]
tokenize input = mapM asToken $ words input

-- >>> asToken "123" == Just (Number 123)
-- >>> asToken "abc" == Nothing
-- >>> tokenize "1 + 2" == Just [Number 1,Plus,Number 2]
-- >>> tokenize "1 + ( 7 - 2 )" == Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
-- >>> tokenize "1 + abc" == Nothing
-- True
-- True
-- True
-- True
-- True
--------------------------------------------------------------------------------

-- >>> return 4 :: [Int]
-- [4]

-- >>> [1, 2] >>= (\x -> [x, x, x])
-- >>> [1, 2] >>= (\x -> [x, x])
-- >>> [1, 2] >>= (\x -> [x])
-- >>> [1, 2] >>= return
-- >>> [1, 2] >>= (\_ -> [])
-- [1,1,1,2,2,2]
-- [1,1,2,2]
-- [1,2]
-- [1,2]
-- []

-- >>> map (\x -> [x, x, x]) [1, 2]
-- >>> concat (map (\x -> [x, x, x]) [1, 2])
-- [[1,1,1],[2,2,2]]
-- [1,1,1,2,2,2]

{-
instance Monad [] where
  return x = [x]
  xs >>= k = concat (map k xs) -- NOTE: or concatMap k xs
  fail _ = []
-}

list :: [(Int, Int)]
list = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

-- NOTE: list comprehension (`list`) is translated to monadic binding (`list''`) at low-level

list' :: [(Int, Int)]
list' = do
  x <- [1, 2, 3]
  y <- [4, 5, 6]
  return (x, y)

list'' :: [(Int, Int)]
list'' =
  [1, 2, 3]
    >>= ( \x ->
            [4, 5, 6]
              >>= ( \y ->
                      return (x, y)
                  )
        )

-- >>> list
-- >>> list'
-- >>> list''
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

{-
TASK
====
Пусть имеется тип данных, который описывает конфигурацию шахматной доски:

```
data Board = ...
```

Кроме того, пусть задана функция

```
nextPositions :: Board -> [Board]
```

которая получает на вход некоторую конфигурацию доски и возвращает все возможные
конфигурации, которые могут получиться, если какая-либо фигура сделает один ход.
Напишите функцию:

```
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
```

которая принимает конфигурацию доски, число ходов `n`, предикат и возвращает все
возможные конфигурации досок, которые могут получиться, если фигуры сделают `n`
ходов и которые удовлетворяют заданному предикату. При `n < 0` функция
возвращает пустой список.

SOLUTION
========
-}

-- NOTE: just for code competition
data Board = Board Int deriving (Show, Eq)

nextPositions :: Board -> [Board]
nextPositions b@(Board x) = [b, Board (x + 1)]

--

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = filter pred [b]
  | otherwise =
    do
      positions <- nextPositions b
      nextPositionsN positions (n - 1) pred

--------------------------------------------------------------------------------

lst :: [(Int, Int)]
lst = [(x, y) | x <- [1, 2, 3], y <- [1, 2], x /= y]

lst' :: [(Int, Int)]
lst' = do
  x <- [1, 2, 3]
  y <- [1, 2]
  True <- return (x /= y) -- NOTE: pattern mathing here
  return (x, y)

lst'' :: [(Int, Int)]
lst'' =
  [1, 2, 3]
    >>= ( \x ->
            [1, 2]
              >>= ( \y ->
                      return (x /= y)
                        >>= ( \b ->
                                case b of
                                  True -> return (x, y)
                                  _ -> fail "..."
                            )
                  )
        )

lst''' :: [(Int, Int)]
lst''' = do
  x <- [1, 2, 3]
  y <- [1, 2]
  if x /= y then "Z" else []
  return (x, y)

-- >>> lst
-- >>> lst'
-- >>> lst''
-- >>> lst'''
-- [(1,2),(2,1),(3,1),(3,2)]
-- [(1,2),(2,1),(3,1),(3,2)]
-- [(1,2),(2,1),(3,1),(3,2)]
-- [(1,2),(2,1),(3,1),(3,2)]

{-
TASK
====
Используя монаду списка и do-нотацию, реализуйте функцию

```
pythagoreanTriple :: Int -> [(Int, Int, Int)]
```

которая принимает на вход некоторое число `x` и возвращает список троек
`(a, b, c)`, таких что

```
a^2 + b^2 = c^2, a>0, b>0, c>0, c≤x, a<b
```

Число `x` может быть ≤0, на таком входе должен возвращаться пустой список.

```
GHCi> pythagoreanTriple 5
[(3,4,5)]
```

```
GHCi> pythagoreanTriple 0
[]
```

```
GHCi> pythagoreanTriple 10
[(3,4,5),(6,8,10)]
```

SOLUTION
========
-}
-- NOTE: imported above
-- import Control.Monad (guard)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise =
    do
      a <- [0 .. x]
      b <- [0 .. x]
      c <- [0 .. x]

      guard (a > 0)
      guard (b > 0)
      guard (c > 0)
      guard (a < b)
      guard (a ^ 2 + b ^ 2 == c ^ 2)

      return (a, b, c)

-- >>> pythagoreanTriple 5 == [(3, 4, 5)]
-- >>> pythagoreanTriple 0 == []
-- >>> pythagoreanTriple 10 == [(3, 4, 5), (6, 8, 10)]
-- True
-- True
-- True

--------------------------------------------------------------------------------
