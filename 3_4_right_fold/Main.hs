{-
Right fold
==================
-}

import Prelude hiding (foldr)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : xs) = x + sumList xs

productList :: [Integer] -> Integer
productList [] = 1
productList (x : xs) = x * productList xs

concatList' :: [[a]] -> [a]
concatList' [] = []
concatList' (x : xs) = x ++ concatList xs

-- >>> sumList [1, 2, 3]
-- >>> productList [1, 2, 3]
-- >>> concatList' [[1, 2, 3], [4, 5, 6]]
-- 6
-- 6
-- [1,2,3,4,5,6]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x : xs) = x `f` foldr f ini xs

sumList' :: [Integer] -> Integer
sumList' = foldr (+) 0

productList' :: [Integer] -> Integer
productList' = foldr (*) 1

-- >>> sumList' [1, 2, 3]
-- >>> productList' [1, 2, 3]
-- 6
-- 6
-- [1,2,3,4,5,6]

{-
TASK
====
Напишите реализацию функции `concatList` через `foldr`

GHCi> concatList [[1,2],[],[3]]
[1,2,3]

SOLUTION
========
-}
concatList :: [[a]] -> [a]
concatList = foldr (++) []

-- >>> concatList [[1,2],[],[3]] == [1,2,3]
-- True
--------------------------------------------------------------------------------

sumPositiveSquares :: [Integer] -> Integer
sumPositiveSquares =
  foldr
    ( \x s ->
        if x > 0
          then x ^ 2 + s
          else s
    )
    0

-- >>> sumPositiveSquares [1, (-2), 3]
-- 10

sumPositiveSquares' :: [Integer] -> Integer
sumPositiveSquares' = foldr f 0
  where
    f x s
      | x > 0 = x ^ 2 + s
      | otherwise = s

-- >>> sumPositiveSquares' [1, (-2), 3]
-- 10

{-
TASK
====
Используя функцию `foldr`, напишите реализацию функции `lengthList`, вычисляющей
количество элементов в списке.

GHCi> lengthList [7,6,5]
3

SOLUTION
========
-}
lengthList :: [a] -> Int
lengthList = foldr f 0
  where
    f _ s = 1 + s

-- >>> lengthList [7,6,5] == 3
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Реализуйте функцию `sumOdd`, которая суммирует элементы списка целых чисел,
имеющие нечетные значения:

GHCi> sumOdd [2,5,30,37]
42

SOLUTION
========
-}
sumOdd :: [Integer] -> Integer
sumOdd = foldr f 0
  where
    f x s
      | odd x = x + s
      | otherwise = s

-- >>> sumOdd [2,5,30,37] == 42
-- True
--------------------------------------------------------------------------------

-- >>> foldr (-) 5 [1, 2, 3]
-- >>> (1 - (2 - (3 - 5)))
-- -3
-- -3

{-
foldr f ini 1:2:3:[]
~> 1 `f` foldr f ini (2:3:[])
~> 1 `f` (2 `f` foldr f ini (3:[]))
~> 1 `f` (2 `f` (3 `f` foldr f ini []))
~> 1 `f` (2 `f` (3 `f` ini))

1:2:3: [ ]
 ↓ ↓ ↓  ↓
 f f f ini
-}

{-
TASK
====
Какой функции стандартной библиотеки, суженной на списки, эквивалентно выражение
`foldr (:) []`?

SOLUTION
========
id
-}

-- >>> foldr (:) [] [1, 2, 3] == id [1, 2, 3]
-- True

{-
TASK
====
Какой функции стандартной библиотеки эквивалентно выражение
`foldr const undefined`?

SOLUTION
========
head
-}

-- >>> foldr const undefined [1, 2, 3] == head [1, 2, 3]
-- True
--------------------------------------------------------------------------------
