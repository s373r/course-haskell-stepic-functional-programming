{-
Basic types
===========

GHCi:
:type (or -t) to show a type
> :type 'c'
'c' :: Char
> :type '\n'
'\n' :: Char

> :type True
True :: Bool
> :type False
False :: Bool

> :type 3
3 :: Num a => a

> let x = 3 :: Int
> x
3
> :type x
x :: Int

> let y = 3 :: Double
> y
3.0
> :type y
y :: Int

> let z = y + 17
> :type z
z :: Double

> :type 3.5
3.5 :: Fractional a => a

> 123456788888888888888888888888888888888 :: Integer
123456788888888888888888888888888888888
-}
import Data.Char

{-
TASK
====
Какие из следующих выражений типизированы верно, то есть не приводят
к ошибкам типа?

SOLUTION
========
[x] (3 :: Integer) + (5 :: Integer)
[ ] (3.0 :: Integer) + (5 :: Integer)
[x] (3.2 :: Double) + (5 :: Double)
[ ] (3 :: Int) + (5 :: Integer)
[ ] (3 :: Double) + (5 :: Float)
-}

-- >>> not False
-- True

{-
> :t not
not :: Bool -> Bool
-}

-- >>> (&&) False True
-- False

{-
:t (&&)
(&&) :: Bool -> Bool -> Bool
-}

{-
TASK
====
Вспомним функцию discount, которая возвращала итоговую сумму покупки
с возможной скидкой. В качестве параметров ей передавались сумма без скидки sum,
процент скидки proc, причем скидка начислялась, если переданная сумма превышает
порог limit. Все эти параметры, как и возвращаемое значение, можно хранить
в типе Double. (Здесь следует отметить, что в реальных финансовых приложениях
использовать тип с плавающей точкой для хранения подобной информации
не рекомендуется.) Тип функции можно задать в файле исходного кода вместе
с ее определением:
> discount :: Double -> Double -> Double -> Double
> discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

Отметим, что объявление типа необязательно, хотя часто рекомендуется
в качестве документации. Его обычно располагают перед определением функции,
хотя это объявление верхнего уровня можно расположить в любом месте файла
с исходным кодом.

Запишите тип функции standardDiscount, определенной как частичное применение
функции discount:
> standardDiscount :: ???
> standardDiscount = discount 1000 5

SOLUTION
========
-}
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

-- >>> standardDiscount 2000 == 1900.0
-- >>> standardDiscount 800  == 800.0
-- True
-- True
--------------------------------------------------------------------------------

test = isDigit '7'

-- >>> test
-- True

{-
TASK
====
Воспользовавшись справочной системой Hoogle, найдите имя функции типа
Char -> Char, переводящей символ в нижний регистр.

SOLUTION
========
toLower
-}

{-
TASK
====
Реализуйте функцию twoDigits2Int, которая принимает два символа
и возвращает число, составленное из этих символов, если оба символа числовые,
и 100 в противном случае. (Первый символ рассматривается как количество
десятков, второй — единиц.)

GHCi> twoDigits2Int '4' '2'
42

SOLUTION
========
-}
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int a b =
  if isDigit a && isDigit b
    then digitToInt a * 10 + digitToInt b
    else 100

-- >>> twoDigits2Int '4' '2' == 42
-- True
--------------------------------------------------------------------------------

{-
Tuples
------
are heterogeneous -- each element can have its own type

Tuples have fixed size
-}

-- >>> (2, True)
-- (2,True)

-- >>> (2, True, 'c')
-- (2,True,'c')

-- >>> fst (2, True)
-- 2

-- >>> snd (2, True)
-- True

{-
> :t ('x', True)
('x',True) :: (Char, Bool)

> :t ('x', True, 's')
('x',True, 's') :: (Char, Bool, Char)
-}

-- >>> (3)
-- 3

-- >>> ()
-- ()

{-
> :t ()
() :: ()
-}

{-
TASK
====
Будем задавать точки на плоскости парами типа (Double, Double).
Реализуйте функцию dist, которая возвращает расстояние между двумя точками,
передаваемыми ей в качестве аргументов.

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = ???

SOLUTION
========
-}
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2)

-- >>> dist (1, 3) (3, 3) == 2.0
-- >>> dist (3, 0) (3, 3) == 3.0
-- True
-- True
--------------------------------------------------------------------------------

{-
Lists
-----
are homogeneous -- all elements have same type

Lists can have any size
-}

-- >>> [1, 2, 3]
-- [1,2,3]

-- >>> [False, True]
-- [False,True]

{-
> :t [False, True]
[False,True] :: [Bool]
-}

-- >>> ['H', 'i']
-- "Hi"

{-
> :t ['H', 'i']
['H','i'] :: [char]
-}

-- >>> "Hi"
-- "Hi"

{-
> :t "Hi"
['H','i'] :: [char]
-}

-- String is a synonym for [Char]
-- >>> "Hi" :: String
-- "Hi"

{-
: Operator adds an element to the beginning (head) of a list
------------------------------------------------------------
-}

str = 'H' : "ello"

-- >>> str
-- "Hello"

{-
Lists concatenation
-------------------
-}

-- >>> str ++ " world"
-- "Hello world"

{-
TASK
====
Операторы (:) и (++) имеют одинаковую ассоциативность и приоритет.
Укажите их. (Воспользуйтесь командой интерпретатора GHCi :info).

SOLUTION
========
( ) infixl 5
(x) infixr 5
( ) infixr 6
( ) infix 5
( ) infix 6
( ) infixl 6
-}

{-
TASK
====
Не используя GHCi, выберите выражения, проходящие проверку типов.

SOLUTION
========
[ ] (++) [1,2] 3 : [4,5,6]
[x] 1 : [2,3] ++ [4,5,6]
[ ] (:) 1 (++) [2,3] [4,5,6]
[x] [1,2] ++ 3 : [4,5,6]
[x] [1,2] ++ (:) 3 [4,5,6]
[ ] [1,2] ++ [3,4,5] : 6
[ ] [1,2] : 3 ++ [4,5,6]
[x] (:) 1 ((++) [2,3] [4,5,6])
-}
