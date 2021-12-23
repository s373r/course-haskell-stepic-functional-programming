{-
Types with parameters
=====================
-}

import Data.Char (isDigit)
import Data.Complex
import Data.List (find)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Text (Text, pack, splitOn, unpack)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

data CoordD = CoordD Double Double

data CoordI = CoordI Int Int

data Coord a = Coord a a
  deriving
    ( Show,
      Eq
    )

intCoord :: Coord Int
intCoord = Coord (3 :: Int) (3 :: Int)

dubleCoord = Coord (3.5 :: Double) (3.5 :: Double)

-- NOTE: we can specify type param in declaration
intCoord' :: Coord Int
intCoord' = Coord 1 1

{-
TASK
====
Реализуйте функции `distance`, считающую расстояние между двумя точками
с вещественными координатами, и `manhDistance`, считающую манхэттенское
расстояние между двумя точками с целочисленными координатами.

SOLUTION
========
-}
-- NOTE: has declared above
-- data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) =
  abs (x2 - x1) + abs (y2 - y1)

--------------------------------------------------------------------------------

{-
TASK
====
Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям
координат. Координаты углов ячейки с координатой `(0,0)` имеют неотрицательные
координаты. Один из углов этой ячейки имеет координату `(0,0)`. С ростом
координат ячеек увеличиваются координаты точек внутри этих ячеек.

Реализуйте функции `getCenter`, которая принимает координату ячейки и возвращает
координату ее центра, и функцию getCell, которая принимает координату точки
и возвращает номер ячейки в которой находится данная точка. В качестве первого
аргумента обе эти функции принимают ширину ячейки.

SOLUTION
========
-}
-- NOTE: has declared above
-- data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = Coord x' y'
  where
    toCenterCoordinate c = (fromIntegral c + 0.5) * width
    x' = toCenterCoordinate x
    y' = toCenterCoordinate y

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = Coord x' y'
  where
    toCellCoordinate c = floor (c / width)
    x' = toCellCoordinate x
    y' = toCellCoordinate y

-- >>> getCell 1 (Coord (-1) (-1)) == Coord (-1) (-1)
-- >>> getCell 1 (Coord 10 10) == Coord 10 10
-- >>> getCenter 8 (Coord (-1) (-1)) == Coord (-4.0) (-4.0)
-- >>> getCenter 1 (Coord (-1) (-1)) == Coord (-0.5) (-0.5)
-- >>> getCell 1 (Coord 1 1) == Coord 1 1
-- >>> getCell 10 (Coord 23 47) == Coord 2 4
-- >>> getCenter 5.0 (Coord 2 3) == Coord 12.5 17.5
-- >>> getCell 1 (Coord 0.5 0) == Coord 0 0
-- >>> getCell 1 (Coord 1 1) == Coord 1 1
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True

-- >>> getCenter 1 (Coord 0 0) == Coord 0.5 0.5
-- >>> getCell 10 (Coord 23 47) == Coord 2 4
-- True
-- True

--------------------------------------------------------------------------------

twice :: a -> [a] -- "[a]" == "[] a"
twice x = [x, x]

thrice :: a -> (,,) a a a
thrice x = (,,) x x x

-- > :t thrice
-- thrice :: a -> (a, a, a)

-- "a -> b" == "(->) a b"

id' :: (->) a a
id' x = x

-- k :: a -> b -> a
-- k :: a -> (b -> a)
-- k :: (->) a (b -> a)
k :: (->) a ((->) b a)
k x y = x

-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

-- > :t Left "ABC"
-- Left "ABC" :: Either [Char] b

-- > :t Right True
-- Left True :: Either a Bool

roots :: Double -> Double -> Double -> Either [Char] (Double, Double)
roots a b c
  | discr >= 0 = Right (x1, x2)
  | otherwise = Left "Negative discrimimant"
  where
    x1 = helper (- d)
    x2 = helper d
    helper x = (- b + x) / (2 * a)
    d = sqrt discr
    discr = b ^ 2 - 4 * a * c

{-
TASK
====
Реализуйте функцию, которая ищет в строке первое вхождение символа, который
является цифрой, и возвращает `Nothing`, если в строке нет цифр.

SOLUTION
========
-}
-- NOTE: has imported above
-- import Data.Char (isDigit)
-- import Data.List (find)

findDigit :: [Char] -> Maybe Char
findDigit = find isDigit

--------------------------------------------------------------------------------

{-
TASK
====
Реализуйте функцию `findDigitOrX`, использующую функцию `findDigit` (последнюю
реализовывать не нужно). `findDigitOrX` должна находить цифру в строке, а если
в строке цифр нет, то она должна возвращать символ `'X'`. Используйте
конструкцию case.

SOLUTION
========
-}
-- NOTE: has imported above
-- import Data.Char(isDigit)
-- import Data.Maybe (fromMaybe)

-- NOTE: implemented above
-- findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX xs = fromMaybe 'X' (findDigit xs)

--------------------------------------------------------------------------------

{-
TASK
====
`Maybe` можно рассматривать как простой контейнер, например, как список
длины 0 или 1. Реализовать функции `maybeToList` и `listToMaybe`,
преобразующие `Maybe a` в `[a]` и наоборот (вторая функция отбрасывает все
элементы списка, кроме первого).

SOLUTION
========
-}
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

listToMaybe :: [a] -> Maybe a
listToMaybe (x : xs) = Just x
listToMaybe _ = Nothing

--------------------------------------------------------------------------------

{-
TASK
====
Реализуйте функцию `parsePerson`, которая разбирает строки вида
`firstName = John\nlastName = Connor\nage = 30` и возвращает либо результат
типа `Person`, либо ошибку типа `Error`.

- Строка, которая подается на вход, должна разбивать по символу `'\n'` на список
  строк, каждая из которых имеет вид X = Y. Если входная строка не имеет
  указанный вид, то функция должна возвращать `ParsingError`.
- Если указаны не все поля, то возвращается `IncompleteDataError`.
- Если в поле age указано не число, то возвращается `IncorrectDataError str`,
  где `str` — содержимое поля `age`.
- Если в строке присутствуют лишние поля, то они игнорируются.

SOLUTION
========
-}
-- NOTE: has imported above
-- import Data.Map (Map, fromList, lookup)
-- import Data.Text (Text, pack, splitOn, unpack)
-- import Text.Read (readMaybe)
-- import Prelude hiding (lookup)

data Error
  = ParsingError
  | IncompleteDataError
  | IncorrectDataError String

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }

parsePerson :: String -> Either Error Person
parsePerson input = do
  map <- toMap input
  toPerson map
  where
    toMap :: String -> Either Error (Map String String)
    toMap x =
      fmap
        fromList
        ( mapM
            (toPair . splitOn (pack " = "))
            (splitOn (pack "\n") (pack x))
        )
      where
        toPair :: [Text] -> Either Error (String, String)
        toPair xs = case xs of
          (a : b : _) -> Right (unpack a, unpack b)
          _ -> Left ParsingError
    toPerson :: Map String String -> Either Error Person
    toPerson map = do
      firstName <- tryGet "firstName" map
      lastName <- tryGet "lastName" map
      age <- tryGetInt "age" map

      return (Person firstName lastName age)
      where
        tryGet :: String -> Map String String -> Either Error String
        tryGet key map = case lookup key map of
          Just value -> Right value
          Nothing -> Left IncompleteDataError

        tryGetInt :: String -> Map String String -> Either Error Int
        tryGetInt key map = do
          value <- tryGet key map
          tryParseToInt value
          where
            tryParseToInt :: String -> Either Error Int
            tryParseToInt value = case readMaybe value of
              Just result -> Right result
              Nothing -> Left $ IncorrectDataError value

--------------------------------------------------------------------------------

-- >>> :t 'c'
-- 'c' :: Char

-- >>> :kind Char
-- Char :: *

-- >>> :kind Int
-- Int :: *

-- >>> :k Maybe
-- Maybe :: * -> *

-- >>> :k Maybe Int
-- Maybe Int :: *

-- >>> :k []
-- [] :: * -> *

-- >>> :k [] Int
-- [] Int :: *

-- >>> :k [Int]
-- [Int] :: *

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,,,)
-- (,,,) :: * -> * -> * -> * -> *

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (->) Char [Char]
-- (->) Char [Char] :: *

{-
TASK
====
Укажите вид конструктора типов Either (Maybe Int).

SOLUTION
========
( ) *
(x) * -> *
( ) * -> * -> *
( ) Это выражение не является корректным конструктором типов
-}

--------------------------------------------------------------------------------

{-
TASK
====
Исправьте ошибку в приведенном коде.

```
eitherToMaybe :: Either a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing
```

SOLUTION
========
-}
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

--------------------------------------------------------------------------------

{-
TASK
====
Укажите все выражения, имеющие вид `*`.

SOLUTION
========
[x] Int -> Int
[x] (Maybe Int, Either (Int -> (Char, Char)) Int)
[x] Maybe (Int -> Either Int Int)
[ ] Either (Int -> Int) Maybe
[ ] Nothing
[x] Maybe Int -> Int
[ ] Either (Int -> (,)) Int
[ ] Either True False
[ ] Maybe -> Int
-}

--------------------------------------------------------------------------------

data CoordLazy a = CoordLazy a a
  deriving (Show)

data CoordStrict a = CoordStrict !a !a
  deriving (Show)

getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x

getXStrict :: CoordStrict a -> a
getXStrict (CoordStrict x _) = x

-- >>> getXLazy (CoordLazy 3 5)
-- >>> getXStrict (CoordStrict 3 5)
-- 3
-- 3

-- >>> getXLazy (CoordLazy 3 undefined)
-- >>> getXStrict (CoordStrict 3 undefined)
-- 3
-- Prelude.undefined

-- NOTE: Data.Complex & Data.Ratio are strict

-- >>> 2 :+ 5 -- Complex
-- 2 :+ 5

-- NOTE: `:+` is a infix data constructor
{-
data Complex = !a :+ !a
data Ratio = !a :% !a
-}

-- >>> 2 `CoordStrict` 5
-- CoordStrict 2 5

{-
TASK
====
Допустим тип `Coord` определен следующим образом:

```
data Coord a = Coord a !a
```

Пусть определены следующие функции:

```
getX :: Coord a -> a
getX (Coord x _) = x

getY :: Coord a -> a
getY (Coord _ y) = y
```

Какие из следующих вызовов  вернут число `3`?

SOLUTION
========
[ ] getY (Coord 3 undefined)
[-] getY (Coord undefined 3)
[ ] getX (Coord undefined undefined)
[ ] getX (Coord undefined 3)
[ ] getY undefined
[ ] getY (Coord 3 7)
[-] getX (Coord 3 3)
[ ] getX (Coord 3 undefined)
-}

--------------------------------------------------------------------------------
