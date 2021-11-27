{-
Parametric polymorphism
=======================
-}
import Data.Function (on)

id' x = x

-- > :t id'
-- id' :: t -> t

-- >>> id' True
-- True

-- >>> id' 5
-- 5

-- >>> (id' id') 4
-- 4

-- > :t id' True
-- id' True :: Bool

-- > :t (id' id')
-- (id' id') :: t -> t

-- NOTE: `k` has already defined as `const` in the standard library
k x y = x

-- >>> k 42 True
-- 42

-- >>> k 42 55
-- 42

-- > :t k
-- k :: t1 -> t2 -> t1

-- > :t const True
-- const True :: b -> Bool

-- > :t undefined
-- undefined :: a

{-
TASK
====
Напишите функцию трех аргументов getSecondFrom, полиморфную по каждому из них,
которая полностью игнорирует первый и третий аргумент, а возвращает второй.
Укажите ее тип.

GHCi> getSecondFrom True 'x' "Hello"
'x'
GHCi> getSecondFrom 'x' 42 True
42

SOLUTION
========
-}
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom _ b _ = b

-- >>> getSecondFrom True 'x' "Hello" == 'x'
-- >>> getSecondFrom 'x' 42 True      == 42
-- True
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Сколько разных всегда завершающихся функций с типом a -> a -> b -> a -> a
можно реализовать?

Две функции одинаковой арности считаются разными, если существует набор значений
их аргументов, на котором они дают разные результирующие значения.

SOLUTION
========
3
-}

mono :: Char -> Char
mono x = x

-- >>> mono 'x'
-- 'x'

-- >>> mono True
-- Couldn't match expected type ‘Char’ with actual type ‘Bool’

-- NOTE: x is a monomorphized parameter (has specific `Char` type)
semiMono :: Char -> a -> Char
semiMono x y = x

-- >>> semiMono 'x' True
-- 'x'

-- NOTE: Type inference algorithm named Hindley–Milner used in Haskell
--       (also known as Damas–Milner or Damas–Hindley–Milner)

{-
Higher-order functions (HOC)
============================
-}

-- > :t ($)
-- ($) :: (a -> b) -> a -> b

apply2 f x = f (f x)

-- > :t apply2
-- apply2 :: (t -> t) -> t -> t

-- >>> apply2 (+5) 22
-- 32

-- >>> apply2 (++ "AB") "CD"
-- "CDABAB"

flip' f x y = f y x

-- >>> flip' (/) 4 2
-- 0.5

-- >>> (/) 4 2
-- 2.0

-- >>> flip const 5 True
-- True

-- > :t flip
-- flip :: (a -> b -> c) -> b -> a -> c

-- > :t flip const
-- flip const :: b -> c -> c

{-
TASK
====

В модуле Data.Function определена полезная функция высшего порядка

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

Она принимает четыре аргумента: бинарный оператор с однотипными аргументами
(типа b), функцию f :: a -> b, возвращающую значение типа b, и два значения
типа a. Функция on применяет f дважды к двум значениям типа a и передает
результат в бинарный оператор.

Используя on можно, например, записать функцию суммирования квадратов аргументов
так:
> sumSquares = (+) `on` (^2)

Функция multSecond, перемножающая вторые элементы пар, реализована следующим
образом
> multSecond = g `on` h
> g = undefined
> h = undefined

Напишите реализацию функций g и h.

> GHCi> multSecond ('A',2) ('E',7)
> 14

SOLUTION
========
-}
multSecond = g `on` h

g = (*)

h = snd

-- >>> multSecond ('A',2) ('E',7) ==14
-- True
--------------------------------------------------------------------------------

{-
Anonymous functions (Lambdas)
-----------------------------
-}

-- >>> 2 * x + 7
-- Variable not in scope: x

f x = 2 * x + 7

-- >>> f 10
-- 27

-- >>> (\x -> 2 * x + 7) 10
-- 27
f' = \x -> 2 * x + 7

-- >>> f' 10
-- 27

lenVec x y = sqrt $ x ^ 2 + y ^ 2

lenVec' x = \y -> sqrt $ x ^ 2 + y ^ 2

lenVec'' = \x -> \y -> sqrt $ x ^ 2 + y ^ 2

lenVec''' = \x y -> sqrt $ x ^ 2 + y ^ 2

-- >>> lenVec 3 4
-- >>> lenVec' 3 4
-- >>> lenVec'' 3 4
-- >>> lenVec''' 3 4
-- 5.0
-- 5.0
-- 5.0
-- 5.0

p1 = ((1, 2), (3, 4))

p2 = ((3, 4), (5, 6))

-- >>> fst $ fst p1
-- 1

sumFstFst = (+) `on` helper
  where
    helper pp = fst $ fst pp

-- >>> sumFstFst p1 p2
-- 4

sumFstFst' = (+) `on` \pp -> fst $ fst pp

-- >>> sumFstFst' p1 p2
-- 4

{-
TASK
====
Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую
в качестве первого аргумента трехместную функцию:

> on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
> on3 op f x y z = undefined

Например, сумма квадратов трех чисел может быть записана с использованием on3
так

> GHCi> let sum3squares = (\x y z -> x+y+z) `on3` (^2)
> GHCi> sum3squares 1 2 3
> 14

SOLUTION
========
-}
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 f op x y z = f (op x) (op y) (op z)

-- >>> sum3squares = (\x y z -> x+y+z) `on3` (^2)
-- >>> sum3squares 1 2 3 == 14
-- True
--------------------------------------------------------------------------------
