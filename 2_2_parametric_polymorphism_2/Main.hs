{-
Parametric polymorphism 2
=========================
-}

import Data.Function (on)

f' :: b -> c
f' = undefined

g' :: a -> b
g' = undefined

x' :: a
x' = undefined

compose f g = \x -> f' (g' x)

-- > :i (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c -- Defined in `GHC.Base`
-- infixr 9 .

sumFstFst'' = (+) `on` (fst . fst)

-- doIt x = f  (g  (h x)) = f ((g . h) x) = (f . (g . h)) x
-- doIt     f . g . h

{-
TASK
====
Функция одной переменной doItYourself выбирает наибольшее из переданного ей
аргумента и числа 42, затем возводит результат выбора в куб и, наконец,
вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована
в виде:
> doItYourself = f . g . h

Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.

> f = undefined
> g = undefined
> h = undefined

SOLUTION
========
-}
f = logBase 2

g = (^ 3)

h = max 42

doItYourself = f . g . h

--------------------------------------------------------------------------------

-- :t [True, False]
-- [True,False] :: [Bool]

-- :t "aqaqa"
-- "aqaqa" :: [Char]

-- :t []
-- [] :: [a]

-- :t (++)
-- (++) :: [a] -> [a] -> [a]

-- :t (:)
-- (:) :: a -> [a] -> [a]

tuple = (True, 3)

tuple' = (,) True 3

tuple'' = (,,) True 3 'c'

-- > :t (,)
-- (,) :: a -> b -> (a, b)

-- > :t (,,)
-- (,,) :: a -> b -> c -> (a, b, c)

-- > :t (,) True 'c'
-- (,) True 'c' :: (Bool, Char)

dup x = (x, x)

-- > :t dup
-- dup :: t -> (t, t)

-- > :t fst
-- fst :: (a, b) -> a

-- > :t snd
-- snd :: (a, b) -> b

{-
TASK
====
Сколько разных всегда завершающихся функций с типом a -> (a,b) -> a -> (b,a,a)
можно реализовать?

SOLUTION
========
9
-}

{-
Carrying
--------
-}

avg :: (Double, Double) -> Double
avg p = (fst p + snd p) / 2

-- >>> fst (1, 2)
-- 1

curry' f x y = f (x, y)

-- :t curry
-- curry' :: ((a, b) -> t) -> a -> b -> t

-- :t uncurry
-- uncurry :: (a -> b -> c)-> (a, b) -> c

{-
TASK
====
Какому известному вам библиотечному оператору, конструктору или функции
эквивалентно выражение `curry id`?

SOLUTION
========
(,)
-}

-- >>> curry id 1 2
-- (1,2)

{-
TASK
====
Какому известному вам библиотечному оператору, конструктору или функции
эквивалентно выражение `uncurry (flip const)`?

SOLUTION
========
snd
-}

{-
TASK
====
В модуле `Data.Tuple` стандартной библиотеки определена функция
`swap :: (a,b) -> (b,a)`, переставляющая местами элементы пары:

> GHCi> swap (1,'A')
> ('A',1)

Эта функция может быть выражена в виде:

> swap = f (g h)

где f, g и h — некоторые идентификаторы из следующего набора:

> curry uncurry flip (,) const

Укажите через запятую подходящую тройку f,g,h.

SOLUTION
========
uncurry,flip,(,)
-}
