{-
Higher-order functions on lists
=============
-}
import Data.Char
  ( isDigit,
    isLower,
  )
import Data.List (sort)
import Prelude hiding
  ( all,
    and,
    any,
    break,
    concat,
    concatMap,
    dropWhile,
    filter,
    map,
    or,
    span,
    takeWhile,
    zipWith,
    zipWith3,
  )

-- NOTE: added `tail` into the proposed example
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : tail
  | otherwise = tail
  where
    tail = filter p xs

-- >>> filter (< 3) [1, 2, 3, 4, 1, 2, 3, 4]
-- [1,2,1,2]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

-- >>> takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4]
-- [1,2]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

-- >>> dropWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4]
-- [3,4,1,2,3,4]

span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

-- >>> span (< 3) [1, 2, 3, 4, 1, 2, 3, 4]
-- ([1,2],[3,4,1,2,3,4])

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

-- >>> break (> 3) [1, 2, 3, 4, 1, 2, 3, 4]
-- ([1,2,3],[4,1,2,3,4])

{-
TASK
====
Напишите функцию `readDigits`, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки,
а второй - ее оставшуюся часть.

GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","")

В решении вам поможет функция `isDigit` из модуля `Data.Char`.

SOLUTION
========
-}
-- NOTE: It has been commented since already imported
-- import Data.Char (isDigit)

readDigits :: String -> (String, String)
readDigits = span isDigit

-- >>> readDigits "365ads" == ("365","ads")
-- >>> readDigits "365"    == ("365","")
-- True
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Реализуйте функцию `filterDisj`, принимающую два унарных предиката и список,
и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]

SOLUTION
========
-}
-- 1)
-- filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
-- filterDisj _ _ [] = []
-- filterDisj p1 p2 (x : xs)
--   | p1 x || p2 x = x : tail
--   | otherwise = tail
--   where
--     tail = filterDisj p1 p2 xs

-- 2)
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

-- >>> filterDisj (< 10) odd [7,8,10,11,12] == [7,8,11]
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Напишите реализацию функции `qsort`. Функция `qsort` должная принимать
на вход список элементов и сортировать его в порядке возрастания
с помощью сортировки Хоара: для какого-то элемента `x` изначального списка
(обычно выбирают первый) делить список на элементы меньше и не меньше `x`,
и потом запускаться рекурсивно на обеих частях.

GHCi> qsort [1,3,2,5]
[1,2,3,5]

Разрешается использовать только функции, доступные из библиотеки `Prelude`.

SOLUTION
========
-}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort ls ++ x : qsort rs
  where
    ls = filter (< x) xs
    rs = filter (>= x) xs

-- >>> qsort [1,3,2,5] == [1,2,3,5]
-- True
--------------------------------------------------------------------------------

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- >>> map (+10) [1, 2, 3, 5]
-- [11,12,13,15]

-- >>> map (^2) [1, 2, 3, 5]
-- [1,4,9,25]

-- >>> map length ["aa", "bbb", "ccccccc"]
-- [2,3,7]

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

-- >>> concat ["Hello", " ", "world", "!"]
-- "Hello world!"

concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap f xs = concat (map f xs)
concatMap f = concat . map f

-- >>> concatMap (\x -> [x, x, x]) "ABCD"
-- "AAABBBCCCDDD"

{-
TASK
====
Напишите функцию `squares'n'cubes`, принимающую список чисел,
и возвращающую список квадратов и кубов элементов исходного списка.

GHCi> squares'n'cubes [3,4,5]
[9,27,16,64,25,125]

SOLUTION
========
-}
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

-- >>> squares'n'cubes [3,4,5] == [9,27,16,64,25,125]
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Воспользовавшись функциями `map` и `concatMap`, определите функцию `perms`,
которая возвращает все перестановки, которые можно получить из данного списка,
в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Считайте, что все элементы в списке уникальны, и что для пустого списка имеется
одна перестановка.

SOLUTION
========
-}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (insert x) (perms xs)
  where
    insert x [] = [[x]]
    insert x ys@(y : ys') = (x : ys) : map (y :) (insert x ys')

-- >>> sort (perms [1,2,3]) == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- True
--------------------------------------------------------------------------------

and, or :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs
or [] = False
or (x : xs) = x || or xs

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- >>> all odd [1, 3, 43]
-- >>> all odd [1, 3, 43, 2]
-- True
-- False

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- >>> any odd [1, 3, 43, 2]
-- >>> any even [1, 3, 43]
-- True
-- False

-- >>> words "Abc in not ABC"
-- ["Abc","in","not","ABC"]

-- >>> unwords (words "Abc in not ABC")
-- >>> unwords . words $ "Abc in not ABC"
-- "Abc in not ABC"
-- "Abc in not ABC"

-- >>> unwords . map reverse . words $ "Abc in not ABC"
-- "cbA ni ton CBA"

revWords :: String -> String
revWords = unwords . map reverse . words

-- >>> revWords "Abc in not ABC"
-- "cbA ni ton CBA"

{-
TASK
====
Реализуйте функцию `delAllUpper`, удаляющую из текста все слова, целиком
состоящие из символов в верхнем регистре. Предполагается, что текст состоит
только из символов алфавита и пробелов, знаки пунктуации,
цифры и т.п. отсутствуют.

GHCi> delAllUpper "Abc IS not ABC"
"Abc not"

Постарайтесь реализовать эту функцию как цепочку композиций, аналогично
`revWords` из предыдущего видео

SOLUTION
========
-}
-- NOTE: It has been commented since already imported
-- import Data.Char (isLower)

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

-- >>> delAllUpper "Abc IS not ABC" == "Abc not"
-- True
--------------------------------------------------------------------------------

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- >>> zipWith (+) [1, 2] [3, 4, 5]
-- [4,6]

-- >>> zipWith (,) [1, 2] [3, 4, 5] -- zip = zipWith (,)
-- [(1,3),(2,4)]

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (x : xs) (y : ys) (z : zs) = f x y z : zipWith3 f xs ys zs
zipWith3 _ _ _ _ = []

{-
TASK
====
Напишите функцию `max3`, которой передаются три списка одинаковой длины
и которая возвращает список той же длины, содержащий на k-ой позиции наибольшее
значение из величин на этой позиции в списках-аргументах.

GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]
GHCi> max3 "AXZ" "YDW" "MLK"
"YXZ"

SOLUTION
========
-}
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 $ (max .) . max

-- >>> max3 [7,2,9] [3,6,8] [1,8,10] == [7,8,10]
-- >>> max3 "AXZ" "YDW" "MLK" == "YXZ"
-- True
-- True
--------------------------------------------------------------------------------
