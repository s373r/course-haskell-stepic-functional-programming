{-
Left fold and comparison with right fold
========================================
-}
import Prelude hiding (foldl)

{-
foldr f ini 1:2:3:[] ~~> 1 `f` (2 `f` (3 `f` ini))
--
((ini `f` 1) `f` 2) `f` 3 == f (f (f ini 1) 2) 3
--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b
-}

{-
TASK
====
При каком значении переменной `x` следующие два выражения примут одно и то же
значение (отличное от неопределенного)?

foldr (-) x [2,1,5]
foldl (-) x [2,1,5]

SOLUTION
========
7
-}
-- >>> x = 7
-- >>> foldr (-) x [2,1,5] == foldl (-) x [2,1,5]
-- True
--------------------------------------------------------------------------------
{-
Lazy
----
-}
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ ini [] = ini
foldl f ini (x : xs) = foldl f (f ini x) xs

{-
foldl f ini 1:2:3:[]
~> foldl f (f ini 1) 2:3:[]
~> foldl f (f (f ini 1) 2) 3:[]
~> foldl f (f (f (f ini 1) 2) 3) []
~> f (f (f (f ini 1) 2) 3)

foldl is NOT recommended to use
-}

{-
Strict
------
Preparation:

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ ini [] = ini
foldl f ini (x : xs) = foldl f ini' xs
  where
    ini' = f ini x
-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ ini [] = ini
foldl' f ini (x : xs) = ini' `seq` foldl' f ini' xs
  where
    ini' = f ini x

-- (||) :: Bool -> Bool -> Bool
-- False || x = x
-- True || _ = True

-- NOTE: foldl (and foldl') cannot be used for infinite lists

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x b -> p x || b) False

-- >>> foldr (\x (s, p) -> (x + s, x * p)) (0, 1) [1, 2, 3, 4]
-- (10,24)

{-
TASK
====
Реализуйте функцию `meanList`, которая находит среднее значение элементов
списка, используя однократный вызов функции свертки.

GHCi> meanList [1,2,3,4]
2.5

Постобработка считается допустимой, то есть предполагаемая реализация функции
`meanList` имеет вид

meanList = someFun . foldr someFoldingFun someIni

SOLUTION
========
-}
meanList :: [Double] -> Double
meanList = mean . foldr f (0, 0)
  where
    f x (s, n) = (x + s, 1 + n)
    mean (s, n) = s / n

-- >>> meanList [1,2,3,4] == 2.5
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Используя однократный вызов свертки, реализуйте функцию `evenOnly`, которая
выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только
четные.

GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"

SOLUTION
========
-}
evenOnly :: [a] -> [a]
evenOnly xs = foldr f [] (zip [1 ..] xs)
  where
    f (i, x) s
      | even i = x : s
      | otherwise = s

-- >>> evenOnly [1   ..  10] == [2,4,6,8,10]
-- >>> evenOnly ['a' .. 'z'] == "bdfhjlnprtvxz"
-- True
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Попробуйте добиться того, чтобы реализованная вами в прошлом задании функция
`evenOnly` позволяла работать и с бесконечными списками.

То есть, например, запрос на первые три элемента бесконечного списка,
возвращаемого этой функцией, примененной к списку всех натуральных чисел,
должен завершаться:

GHCi> take 3 (evenOnly [1..])
[2,4,6]

SOLUTION
========
the same as before
-}

-- >>> take 3 (evenOnly [1..]) == [2,4,6]
-- True
--------------------------------------------------------------------------------
