{-
Functions for working with lists
================================
-}
import Prelude hiding
  ( drop,
    init,
    last,
    length,
    null,
    reverse,
    splitAt,
    take,
    unzip,
    zip,
    zip3,
    (!!),
    (++),
  )

-- >>> []
-- []

-- >>> 3 : []
-- [3]

lst = 5 : 3 : []

-- >>> lst
-- [5,3]

-- >>> 7 : lst
-- [7,5,3]

-- >>> [5, 3] == lst
-- True

cons42 = (42 :)

-- > :t cons42
-- cons42 :: [Integer] -> [Integer]

-- >>> cons42 [1, 2, 3]
-- [42,1,2,3]

{-
TASK
====
Реализуйте функцию `addTwoElements`, которая бы добавляла два переданных
ей значения в голову переданного списка.

GHCi> addTwoElements 2 12 [85,0,6]
[2,12,85,0,6]

SOLUTION
========
-}
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y l = x : y : l

-- >>> addTwoElements 2 12 [85,0,6] == [2,12,85,0,6]
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Реализуйте функцию `nTimes`, которая возвращает список, состоящий
из повторяющихся значений ее первого аргумента. Количество повторов определяется
значением второго аргумента этой функции.

GHCi> nTimes 42 3
[42,42,42]
GHCi> nTimes 'z' 5
"zzzzz"

SOLUTION
========
-}
nTimes :: a -> Int -> [a]
nTimes x n = addToList [] x n
  where
    addToList list x n
      | n == 0 = list
      | otherwise = addToList (x : list) x (n - 1)

-- >>> nTimes 42 3 == [42,42,42]
-- >>> nTimes 'z' 5 == "zzzzz"
-- True
-- True
--------------------------------------------------------------------------------

-- > :t head
-- head :: [a] -> a

-- >>> head [1, 2 ,3]
-- 1

-- > :t tail
-- tail :: [a] -> a

-- >>> tail [1, 2 ,3]
-- [2,3]

-- >>> head []
-- Prelude.head: empty list

second :: [a] -> a
-- second xs = head (tail xs)
second = head . tail

-- >>> second [1, 2, 3]
-- 2

-- >>> second []
-- Prelude.tail: empty list

fst' :: (a, b) -> a
fst' ((,) x y) = x

-- > :t fst'
-- fst' :: (t, t1) -> t

-- >>> fst' ("Hi", 3)
-- "Hi"

head' :: [a] -> a
head' ((:) x xs) = x

-- >>> head' [1, 2, 3]
-- 1

tail' :: [a] -> [a]
tail' (x : xs) = xs

-- >>> tail' [1, 2, 3]
-- [2,3]

tail'' :: [a] -> [a]
tail'' (_ : xs) = xs

-- >>> tail'' [1, 2, 3]
-- [2,3]

second' :: [a] -> a
second' (_ : xs) = head xs

-- >>> second' [1, 2, 3]
-- 2

second'' :: [a] -> a
second'' (_ : x : _) = x

-- >>> second'' [1, 2, 3]
-- 2

{-
TASK
====
Исследуйте тип функции

> sndHead = snd . head

и разберитесь, каково ее поведение. Эту функцию можно реализовать,
используя сопоставление с образцом

> sndHead некоторый_образец = x

Отметьте те образцы, которые подходят для этой цели.

SOLUTION
========
[ ] ((,) x y : z)
[x] ((_, x) : _)
[x] ((,) y z : x)
[x] ((:) ((,) _ x) y)
[ ] ((,) y x : z)
[ ] ((,) ((:) _ _) x)
-}

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : xs ++ ys -- NOTE: read as (x : xs) ++ ys = x : (xs ++ ys)

null :: [a] -> Bool
null [] = True
null _ = False

{-
TASK
====
Сформируйте список целых чисел, содержащий только те элементы исходного списка,
значение которых нечетно.

GHCi> oddsOnly [2,5,7,10,11,12]
[5,7,11]

Для анализа четности можно использовать функции `odd` и `even` стандартной
библиотеки.

SOLUTION
========
-}
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

-- >>> oddsOnly [2,5,7,10,11,12] == [5,7,11]
-- True
--------------------------------------------------------------------------------

-- NOTE: Complexity is O(N)
last :: [a] -> a
last (x : []) = x
last (_ : xs) = last xs

init :: [a] -> [a]
init [] = error "List is empty!"
init [_] = []
init (x : xs) = x : init xs

-- >>> init [1, 2, 3]
-- [1,2]

-- >>> init []
-- List is empty

reverse :: [a] -> [a]
reverse l = rev l []
  where
    rev [] a = a
    rev (x : xs) a = rev xs (x : a)

-- >>> reverse "ABCD"
-- "DCBA"

{-
TASK
====
Реализуйте функцию `isPalindrome`, которая определяет, является ли переданный ей
список палиндромом.

> GHCi> isPalindrome "saippuakivikauppias"
> True
> GHCi> isPalindrome [1]
> True
> GHCi> isPalindrome [1, 2]
> False

SOLUTION
========
-}
isPalindrome xs = helper xs (reverse xs) n
  where
    n = length xs `div` 2
    helper _ _ 0 = True
    helper (x : xs) (y : ys) n =
      (x == y) && helper xs ys (n - 1)

-- OR
-- isPalindrome xs = reverse xs == xs

-- >>> isPalindrome "saippuakivikauppias" == True
-- >>> isPalindrome [1]                   == True
-- >>> isPalindrome [1, 2]                == False
-- True
-- True
-- True
--------------------------------------------------------------------------------

zip :: [a] -> [b] -> [(a, b)]
zip (a : as) (b : bs) = (a, b) : zip as bs
zip _ _ = []

-- >>> zip [1, 2, 3] "Hello"
-- [(1,'H'),(2,'e'),(3,'l')]

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (a : as) (b : bs) (c : cs) = (a, b, c) : zip3 as bs cs
zip3 _ _ _ = []

-- >>> zip3 "aA" "bB" "cC"
-- [('a','b','c'),('A','B','C')]

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : xys) =
  let (xs, ys) = unzip xys
   in (x : xs, y : ys)

{-
TASK
====
Составьте список сумм соответствующих элементов трех заданных списков. Длина
результирующего списка должна быть равна длине самого длинного из заданных
списков, при этом «закончившиеся» списки не должны давать вклада в суммы.

GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3]

SOLUTION
========
-}
headTail :: Num a => [a] -> (a, [a])
headTail [] = (0, [])
headTail (x : xs) = (x, xs)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 as bs cs = (a + b + c) : sum3 as' bs' cs'
  where
    (a, as') = headTail as
    (b, bs') = headTail bs
    (c, cs') = headTail cs

-- >>> sum3 [1,2,3] [4,5] [6] == [11,7,3]
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Напишите функцию `groupElems` которая группирует одинаковые элементы в списке
(если они идут подряд) и возвращает список таких групп.

> GHCi> groupElems []
[]
> GHCi> groupElems [1,2]
[[1],[2]]
> GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
> GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]

Разрешается использовать только функции, доступные из библиотеки `Prelude`.

SOLUTION
========
-}
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (a : as) = reverse (helper [] [a] as)
  where
    helper result [] [] = result
    helper result group [] = group : result
    helper result group (a : as) =
      if g == a
        then helper result (a : group) as
        else helper (group : result) [a] as
      where
        g = head group

-- >>> groupElems []          == []
-- >>> groupElems [1,2]       == [[1],[2]]
-- >>> groupElems [1,2,2,2,4] == [[1],[2,2,2],[4]]
-- >>> groupElems [1,2,3,2,4] == [[1],[2],[3],[2],[4]]
-- True
-- True
-- True
-- True
--------------------------------------------------------------------------------

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

-- >>> take 5 "Hello World"
-- "Hello"

-- >>> take 5 "Hi"
-- "Hi"

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

-- >>> "Hello World" !! 6
-- 'W'
