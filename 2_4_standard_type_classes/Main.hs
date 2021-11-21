{-
Standard type classes
=====================
-}

-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x /= y = not (x == y)
--   x == y = not (x /= y)
--
-- `Ord a` is a type class extension on `Eq a`
-- (`Ord` means order)
--
-- class (Eq a) => Ord a where
--   (<), (<=), (>=), (>) :: a -> a -> Bool
--   max, min :: a -> a -> a
--   compare :: a -> a -> Ordering
-- NOTE: Minimal complete definition: either `compare` or `<=`

-- > :i ordering
-- data Ordering LT | EQ | GT -- Defined in `GHC.Types`
-- ...

-- Multiple extending:
-- class (Eq a, Printable a) => MyClass where
--   ...

{-
TASK
====
Пусть существуют два класса типов `KnownToGork` и `KnownToMork`, которые
предоставляют методы `stomp (stab)` и `doesEnrageGork (doesEnrageMork)`
соответственно:

> class KnownToGork a where
>     stomp :: a -> a
>     doesEnrageGork :: a -> Bool
>
> class KnownToMork a where
>     stab :: a -> a
>     doesEnrageMork :: a -> Bool

Класса типов `KnownToGorkAndMork` является расширением обоих этих классов,
предоставляя дополнительно метод `stompOrStab`:

> class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
>     stompOrStab :: a -> a

Задайте реализацию по умолчанию метода `stompOrStab`, которая вызывает метод
stomp, если переданное ему значение приводит в ярость Морка; вызывает `stab`,
если оно приводит в ярость Горка и вызывает сначала `stab`, а потом `stomp`,
если оно приводит в ярость их обоих. Если не происходит ничего
из вышеперечисленного, метод должен возвращать переданный ему аргумент.

SOLUTION
========
class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x
    | both = stomp (stab x)
    | mork = stomp x
    | gork = stab x
    | otherwise = x
    where
      mork = doesEnrageMork x
      gork = doesEnrageGork x
      both = mork && gork
-}

-- :t show
-- show :: Shown a => a -> String

-- >>> show 5
-- "5"

-- >>> show 5.0
-- "5.0"

-- >>> show [1, 2]
-- "[1,2]"

-- > :t read
-- read :: Read a => String -> a

-- >>> read 5
-- Could not deduce (Num String) arising from the literal ‘5’
-- from the context: Read a
--   bound by the inferred type of it :: Read a => a

-- >>> read "5" :: Int
-- 5

-- >>> read "5" :: Double
-- 5.0

-- >>> read "[1,2]" :: [Double]
-- [1.0,2.0]

-- >>> read "5 rings" :: Int
-- Prelude.read: no parse

-- >>> reads "5 rings" :: [(Int, String)]
-- [(5," rings")]

-- >>> reads "asd5 rings" :: [(Int, String)]
-- []

{-
TASK
====
Имея функцию `ip = show a ++ show b ++ show c ++ show d` определите значения
`a`, `b`, `c`, `d` так, чтобы добиться следующего поведения:

GHCi> ip
"127.224.120.12"

SOLUTION
========
a = 127.2
b = 24.1
c = 20.1
d = 2
-}

-- class Enum a where
--   succ, pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int

-- >>> succ 4
-- 5

-- >>> pred 4
-- 3

-- >>> pred 'z'
-- 'y'

-- >>> succ 'z'
-- '{'

-- >>> fromEnum 'z'
-- 122

-- >>> toEnum 122
-- Prelude.Enum.().toEnum: bad argument

-- >>> toEnum 122 :: Char
-- 'z'

-- class Bounded a where
--   minBound, maxBound :: a

-- >>> succ False
-- True

-- >>> succ True
-- Prelude.Enum.Bool.succ: bad argument

-- >>> minBound :: Bool
-- >>> maxBound :: Bool
-- False
-- True

-- >>> minBound :: Int
-- >>> maxBound :: Int
-- -9223372036854775808
-- 9223372036854775807

-- >>> minBound :: Char
-- >>> maxBound :: Char
-- '\NUL'
-- '\1114111'

-- >>> maxBound :: Integer
-- No instance for (Bounded Integer) arising from a use of ‘maxBound’

{-
TASK
====
Реализуйте класс типов

> class SafeEnum a where
>   ssucc :: a -> a
>   spred :: a -> a

обе функции которого ведут себя как `succ` и `pred` стандартного класса `Enum`,
однако являются тотальными, то есть не останавливаются с ошибкой на наибольшем
и наименьшем значениях типа-перечисления соответственно, а обеспечивают
циклическое поведение. Ваш класс должен быть расширением ряда классов типов
стандартной библиотеки, так чтобы можно было написать реализацию по умолчанию
его методов, позволяющую объявлять его представителей без необходимости писать
какой бы то ни было код. Например, для типа `Bool` должно быть достаточно
написать строку

> instance SafeEnum Bool

и получить возможность вызывать

GHCi> ssucc False
True
GHCi> ssucc True
False

SOLUTION
========
class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | overflow = minBound
    | otherwise = succ x
    where
      overflow = x == maxBound

  spred :: a -> a
  spred x
    | underflow = maxBound
    | otherwise = pred x
    where
      underflow = x == minBound
-}

-- class Num a where
--   (+), (-), (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--
--   x - y = x + negate y
--   negate x = 0 - x
--
-- LAW: abs x * signum x == x

-- > :t fromInteger 3
-- fromInteger 3 :: Num a => a

-- > :i Integral
-- class (Real a, Enum) => Integral a where
--   quot :: a -> a -> a
--   rev :: a -> a -> a
--   div :: a -> a -> a
--   mod :: a -> a -> a
--   quotRem :: a -> a -> (a, a)
--   dimMod :: a -> a -> (a, a)
--   toInteger :: a -> Integer

-- > :i Fractional
-- class Num a => Fractional a where
--   (/) :: a -> a -> a
--   recip :: a -> a
--   fromRational :: Rational -> a

-- > :i Floating
-- class Fractional a => Floating a where
--   pi :: a
--   exp :: a -> a
--   sqrt :: a -> a
--   log :: a -> a
--   (**) :: a -> a -> a
--   logBase :: a -> a -> a
--   sin :: a -> a
--   cos :: a -> a
--   ...

-- > :i RealFrac
-- class (Real a, Fractional a) => RealFrac a where
--   properFraction :: Integral b => a -> (b, a)
--   truncate :: Integral b => a -> b
--   round :: Integral b => a -> b
--   ceiling :: Integral b => a -> b
--   floor :: Integral b => a -> b

-- > :i RealFloat
-- class (RealFrac a, Floating a) => RealFloat a where
--   floatRadix :: a -> Integer
--   floatDigits :: a -> Int
--   floatRange :: a -> (Int, Int)
--   decodeFloat :: a -> (Integer, Int)
--   encodeFloat :: Integer -> Int -> a
--   exponent :: a -> Int
--   significand :: a -> a
--   scaleFloat :: Int -> a -> a
--   isNaN :: a -> Bool
--   isInfinite :: a -> Bool
--   isDenormalized :: a -> Bool
--   isNegativeZero :: a -> Bool
--   isIEEE :: a -> Bool
--   atan2 :: a -> a -> a

{-
TASK
====
Напишите функцию с сигнатурой:

avg :: Int -> Int -> Int -> Double

вычисляющую среднее значение переданных в нее аргументов:

GHCi> avg 3 4 8
5.0

SOLUTION
========
avg :: Int -> Int -> Int -> Double
avg a b c = sum / 3.0
  where
    sum = fromInteger (toInteger a + toInteger b + toInteger c)
-}
