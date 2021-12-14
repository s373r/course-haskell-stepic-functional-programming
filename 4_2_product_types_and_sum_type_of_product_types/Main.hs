{-
Product types and sum type of product types
===========================================
-}
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', unfoldr)
import Data.Ratio
import Numeric (showIntAtBase)

-- NOTE: named product type
--       `Pt` is a constructor [function]
-- data Point = Pt Double Double deriving (Show)

-- NOTE: usually data type name & ctor are the same:
data Point = Point Double Double deriving (Show)

-- > :t
-- Point :: Double -> Double -> Point

-- >>> Point 3.0 4.0
-- Point 3.0 4.0

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

{-
TASK
====
Реализуйте функцию `distance`, возвращающую расстояние между двумя точками.

SOLUTION
========
-}
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- >>> distance (Point 3 1) (Point 2 1) == 1
-- True
--------------------------------------------------------------------------------

rootsOld :: Double -> Double -> Double -> (Double, Double)
rootsOld a b c = (x1, x2)
  where
    x1 = helper (- d)
    x2 = helper d
    helper x = (- b + x) / (2 * a)
    d = sqrt discr
    discr = b ^ 2 - 4 * a * c

-- >>> rootsOld 1 1 6
-- (NaN,NaN)

data Roots = Roots Double Double | None
  deriving (Show)

roots :: Double -> Double -> Double -> Roots
roots a b c
  | discr >= 0 = Roots x1 x2
  | otherwise = None
  where
    x1 = helper (- d)
    x2 = helper d
    helper x = (- b + x) / (2 * a)
    d = sqrt discr
    discr = b ^ 2 - 4 * a * c

-- >>> roots 1 (-5) 6
-- Roots 2.0 3.0

-- >>> roots 1 1 6
-- None

{-
TASK
====
Определим тип фигур `Shape`:

```
data Shape = Circle Double | Rectangle Double Double
```

У него два конструктора: `Circle r` — окружность радиуса `r`,
и `Rectangle a b` — прямоугольник с размерами сторон `a` и `b`. Реализуйте
функцию `area`, возвращающую площадь фигуры. Константа `pi` уже определена
в стандартной библиотеке.

SOLUTION
========
-}
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Rectangle a b) = a * b

-- NOTE: or
-- area' :: Shape -> Double
-- area' s =
--   case s of
--     (Circle r) -> pi * (r ^ 2)
--     (Rectangle a b) -> a * b
--------------------------------------------------------------------------------

{-
TASK
====
В одном из прошлых заданий мы встречали тип `Result` и функцию `doSomeWork`:

```
data Result = Fail | Success

doSomeWork :: SomeData -> (Result,Int)
```

Функция `doSomeWork` возвращала результат своей работы и либо код ошибки
в случае неудачи, либо `0` в случае успеха. Такое определение функции
не является наилучшим, так как в случае успеха мы вынуждены возвращать некоторое
значение, которое не несет никакой смысловой нагрузки.

Используя функцию `doSomeWork`, определите функцию `doSomeWork'` так, чтобы она
возвращала код ошибки только в случае неудачи. Для этого необходимо определить
тип `Result'`. Кроме того, определите `instance Show для Result'` так,
чтобы `show` возвращал `"Success"` в случае успеха и `"Fail: N"` в случае
неудачи, где `N` — код ошибки.

SOLUTION
========
-}
data Result = Fail | Success

data SomeData

doSomeWork :: SomeData -> (Result, Int)
doSomeWork = undefined

--

data Result' = Ok | Result' Int

instance Show Result' where
  show Ok = "Success"
  show (Result' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case snd $ doSomeWork x of
  0 -> Ok
  n -> Result' n

--------------------------------------------------------------------------------

square :: Double -> Shape
square a = Rectangle a a

-- >>> 2 % 3 -- NOTE: it is ⅔
-- 2 % 3

-- >>> 2 % 3 + 1 % 6
-- 5 % 6

{-
TASK
====
Реализуйте функцию `isSquare`, проверяющую является ли фигура квадратом.

SOLUTION
========
-}
isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False

--------------------------------------------------------------------------------

{-
TASK
====
Целое число можно представить как список битов со знаком.

Реализуйте функции сложения и умножения для таких целых чисел, считая,
что младшие биты идут в начале списка, а старшие — в конце. Можно считать,
что на вход не будут подаваться числа с ведущими нулями.

SOLUTION
========
-}
-- import Data.List (foldl', unfoldr)

data Bit = Zero | One

data Sign = Minus | Plus

data Z = Z Sign [Bit]

fromBit :: (Integral a) => Bit -> a
fromBit Zero = 0
fromBit One = 1

fromBits :: (Integral a) => [Bit] -> [a]
fromBits = map fromBit

fromSign :: (Integral a) => Sign -> a
fromSign Minus = -1
fromSign Plus = 1

fromZ :: (Integral a) => Z -> a
fromZ (Z sign bits) = sign' * value
  where
    sign' = fromSign sign
    value = (toDecimal . fromBits . reverse) bits

intoBit :: (Integral a) => a -> Bit
intoBit 0 = Zero
intoBit 1 = One
intoBit _ = undefined

intoBits :: (Integral a) => [a] -> [Bit]
intoBits = map intoBit

intoSign :: (Integral a) => a -> Sign
intoSign x = case signum x of
  -1 -> Minus
  _ -> Plus

intoZ :: (Integral a) => a -> Z
intoZ x = Z sign bits
  where
    sign = intoSign x
    bits = (intoBits . toReversedBinary) x

toDecimal :: Integral a => [a] -> a
toDecimal = foldl' f 0
  where
    f x acc = x * 2 + acc

toReversedBinary :: (Integral a) => a -> [a]
toReversedBinary = unfoldr f
  where
    f x = case divMod (abs x) 2 of
      (0, 0) -> Nothing
      (div, mod) -> Just (mod, div)

add :: Z -> Z -> Z
add a b = intoZ (fromZ a + fromZ b)

mul :: Z -> Z -> Z
mul a b = intoZ (fromZ a * fromZ b)

--------------------------------------------------------------------------------

fromMaybe (Just x) = x
fromMaybe Nothing = error "!!!"

-- NOTE: lazing pattern mathing
fromMaybe' ~(Just x) = x
fromMaybe' Nothing = error "!!!"

-- >>> fromMaybe Nothing
-- >>> fromMaybe' Nothing
-- !!!
-- Non-exhaustive patterns in Just x

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- (***) f g p = (f $ fst p, g $ snd p)
-- (***) f g (x, y) = (f x, g y)
(***) f g ~(x, y) = (f x, g y)

-- >>> succ *** pred $ (5, 5)
-- (6,4)

-- NOTE: output for
-- (***) f g p = (f $ fst p, g $ snd p)
-- >>> const 1 *** const 2 $ (5, 5)
-- >>> const 1 *** const 2 $ (undefined, undefined)
-- >>> const 1 *** const 2 $ undefined
-- (1,2)
-- (1,2)
-- (1,2)

-- NOTE: output for
-- (***) f g (x, y) = (f x, g y)
-- >>> const 1 *** const 2 $ (5, 5)
-- >>> const 1 *** const 2 $ (undefined, undefined)
-- >>> const 1 *** const 2 $ undefined
-- (1,2)
-- (1,2)
-- Prelude.undefined

-- NOTE: output for
-- (***) f g ~(x, y) = (f x, g y)
-- >>> const 1 *** const 2 $ (5, 5)
-- >>> const 1 *** const 2 $ (undefined, undefined)
-- >>> const 1 *** const 2 $ undefined
-- (1,2)
-- (1,2)
-- (1,2)

{-
TASK
====
Пусть определена следующая функция:

```
foo :: Bool -> Int
foo ~True = 1
foo False = 0
```

Что произойдет при вызове foo False?

SOLUTION
========
[ ] Функция вернет 0
[ ] Поведение не определено
[ ] Будет брошено исключение
[x] Функция вернет 1
[ ] Вычисление зависнет
[ ] Произойдет ошибка при сопоставлении с образцом
-}
