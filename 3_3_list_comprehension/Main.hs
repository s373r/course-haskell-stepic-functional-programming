{-
List comprehension
==================
-}
import Prelude hiding
  ( cycle,
    iterate,
    repeat,
    replicate,
  )

bot = not bot

-- >>> bot
-- ProgressCancelledException

ones :: [Integer]
ones = 1 : ones

-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1..

nats :: Num t => t -> [t]
nats n = n : nats (n + 1)

-- >>> nats 5
-- [5,6,7,8,9..

-- >>> take 10 $ nats 5
-- [5,6,7,8,9,10,11,12,13,14]

-- >>> head $ nats 42
-- 42

{-
head (x : xs) = x
head [] = error "Empty list"

head $ nats 42
~> head $ (42 : nats (42 + 1)) -- function pattern matching (`x : xs`) evaluate
 |                             -- `x`
~> 42

head $ nats (40 + 2)
~> head $ ((40 + 2) : nats ((40 + 2) + 1))
~> 40 + 2
~> 42
-}

suares :: [Integer]
suares = map (^ 2) $ nats 1

-- >>> take 10 $ suares
-- [1,4,9,16,25,36,49,64,81,100]

{-
TASK
====
еализуйте c использованием функции `zipWith` функцию `fibStream`, возвращающую
бесконечный список чисел Фибоначчи.

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]

SOLUTION
========
-}
fibStream :: [Integer]
fibStream = fibonacci 0 1
  where
    fibonacci prev curr = prev : fibonacci curr (curr + prev)

-- >>> (take 10 $ fibStream) == [0,1,1,2,3,5,8,13,21,34]
-- True
--------------------------------------------------------------------------------

repeat :: a -> [a]
repeat x = xs
  where
    xs = x : xs

-- >>> take 5 $ repeat 'z'
-- "zzzzz"

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

-- >>> replicate 5 'z'
-- "zzzzz"

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = ys
  where
    ys = xs ++ ys

-- >>> take 10 $ cycle [1, 2, 3]
-- [1,2,3,1,2,3,1,2,3,1]

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- >>> take 5 $ iterate (^2) 2
-- [2,4,16,256,65536]

{-
TASK
====
Предположим, что функция `repeat`, была бы определена следующим образом:

`repeat = iterate repeatHelper`

определите, как должна выглядеть функция `repeatHelper`.

SOLUTION
========
-}
repeatHelper :: a -> a
repeatHelper x = x

-- OR:
-- repeatHelper = id

repeat' :: a -> [a]
repeat' = iterate repeatHelper

-- >>> (take 5 $ repeat' 'f') == "fffff"
-- True
--------------------------------------------------------------------------------

-- >>> [1..10] -- enumFromTo 1 10
-- >>> enumFromTo 1 10
-- [1,2,3,4,5,6,7,8,9,10]
-- [1,2,3,4,5,6,7,8,9,10]

-- > :i enumFromTo
-- class Enum a where
--  ...
--  enumFromTo :: a -> a -> [a]
--  ...

-- >>> ['a'..'z']
-- "abcdefghijklmnopqrstuvwxyz"

-- >>> [1, 3 .. 10] -- with step == 2 (3 - 1)
-- >>> enumFromThenTo 1 3 10
-- [1,3,5,7,9]
-- [1,3,5,7,9]

-- >>> take 5 $ [1..]
-- >>> take 5 $ enumFrom 1
-- [1,2,3,4,5]
-- [1,2,3,4,5]

-- >>> take 5 $ [7, 14..]
-- >>> take 5 $ enumFromThen 7 14
-- [7,14,21,28,35]
-- [7,14,21,28,35]

{-
TASK
====
Пусть задан тип `Odd` нечетных чисел следующим образом:

data Odd = Odd Integer
  deriving (Eq, Show)

Сделайте этот тип представителем класса типов `Enum`.

GHCi> succ $ Odd (-100000000000003)
Odd (-100000000000001)

Конструкции с четным аргументом, типа `Odd 2`, считаются недопустимыми
и не тестируются.

Примечание. Мы еще не знакомились с объявлениями пользовательских типов данных,
однако, скорее всего, приведенное объявление не вызовет сложностей. Здесь
объявляется тип данных `Odd` с конструктором `Odd`. Фактически это простая
упаковка для типа `Integer`. Часть `deriving (Eq, Show)` указывает компилятору,
чтобы он автоматически сгенерировал представителей соответствующих классов типов
для нашего типа (такая возможность имеется для ряда стандартных классов типов).
Значения типа `Odd` можно конструировать следующим образом:

GHCi> let x = Odd 33
GHCi> x
Odd 33

и использовать конструктор данных Odd в сопоставлении с образцом:

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

SOLUTION
========
-}
-- NOTE: the following solution is overcomlicated
--       use instead `map Odd [n, n + 2, m]` approach

data Odd = Odd Integer deriving (Eq, Show)

oddStep :: Integer
oddStep = 2

next :: Integer -> Odd -> Odd
next distance (Odd x) = Odd (x + oddStep * distance)

distance :: Odd -> Odd -> Integer
distance (Odd start) (Odd end) = (end - start) `div` oddStep

oddStream :: Odd -> Integer -> [Odd]
oddStream x distance = x : oddStream nextX distance
  where
    nextX = next distance x

instance Enum Odd where
  succ = next 1

  pred = next (-1)

  toEnum = Odd . toInteger

  fromEnum (Odd x) = fromInteger x

  enumFrom start = enumFromThen start second
    where
      second = succ start

  enumFromThen start second = oddStream start distance'
    where
      distance' = distance start second

  enumFromTo start end = enumFromThenTo start second end
    where
      second = succ start

  enumFromThenTo start second (Odd end) =
    takeWhile needNext (oddStream start distance')
    where
      distance' = distance start second

      needNext
        | ascending = (<= end) . value
        | otherwise = (>= end) . value
        where
          ascending = distance' > 0
          value = \(Odd x) -> x

tests :: [(Integer, Bool)]
tests =
  [ (1, succ (Odd 1) == Odd 3),
    (2, pred (Odd 3) == Odd 1),
    (3, take 3 [Odd 1 ..] == [Odd 1, Odd 3, Odd 5]),
    (4, take 3 [Odd 1 .. Odd 7] == [Odd 1, Odd 3, Odd 5]),
    (5, take 3 [Odd 7 .. Odd 1] == []),
    (6, take 3 [Odd 7 .. Odd 1] == []),
    (7, take 3 [Odd 1, Odd 3 ..] == [Odd 1, Odd 3, Odd 5]),
    (8, take 3 [Odd 3, Odd 1 ..] == [Odd 3, Odd 1, Odd (-1)]),
    (9, [Odd 1, Odd 5 .. Odd 7] == [Odd 1, Odd 5]),
    (10, [Odd 7, Odd 5 .. Odd 1] == [Odd 7, Odd 5, Odd 3, Odd 1]),
    (11, [Odd 7, Odd 5 .. Odd 11] == []),
    (12, [Odd 3, Odd 5 .. Odd 1] == []),
    (13, [Odd 1 .. Odd 7] == [Odd 1, Odd 3, Odd 5, Odd 7]),
    (14, take 5 [Odd (10 ^ 20 + 7), Odd (10 ^ 20 + 11) .. Odd (10 ^ 20 + 17)] == [Odd 100000000000000000007, Odd 100000000000000000011, Odd 100000000000000000015]),
    (15, take 5 [Odd (10 ^ 20 + 17), Odd (10 ^ 20 + 13) .. Odd (10 ^ 20 + 7)] == [Odd 100000000000000000017, Odd 100000000000000000013, Odd 100000000000000000009])
  ]

failedTests :: [Integer]
failedTests = map fst $ filter (not . snd) tests

runTests :: [Char]
runTests
  | null failedTests = "All tests passed!"
  | otherwise = "Failed tests: " ++ show failedTests

-- >>> runTests
-- "All tests passed!"
--------------------------------------------------------------------------------
xs = [1 .. 20]

-- >>> [x ^ 2 | x <- xs]
-- [1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400]

-- >>> [x ^ 2 | x <- xs, x ^ 2 < 200]
-- [1,4,9,16,25,36,49,64,81,100,121,144,169,196]

-- >>> [(x, y) | x <- [1, 2], y <- [1, 2]]
-- [(1,1),(1,2),(2,1),(2,2)]

pythagoreanTriples =
  [ (x, y, z)
    | x <- xs,
      y <- xs,
      z <- xs,
      x ^ 2 + y ^ 2 == z ^ 2,
      x <= y
  ]

-- >>> pythagoreanTriples
-- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]

{-
TASK
====
Пусть есть список положительных достоинств монет `coins`, отсортированный
по возрастанию. Воспользовавшись механизмом генераторов списков, напишите
функцию `change`, которая разбивает переданную ей положительную сумму денег
на монеты достоинств из списка `coins` всеми возможными способами.
Например, если `coins = [2, 3, 7]`:

GHCi> change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]

Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы
`[2,2,3]` и `[2,3,2]` — различаются.
Список `coins` определять не надо.

SOLUTION
========
-}
coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Num a, Ord a) => a -> [[a]]
change 0 = [[]]
change n =
  [ c : rest
    | c <- coins,
      n - c >= 0,
      rest <- change (n - c)
  ]

-- >>> (change 7) == [[2,2,3],[2,3,2],[3,2,2],[7]]
-- True
--------------------------------------------------------------------------------
