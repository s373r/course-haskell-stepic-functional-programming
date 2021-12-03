{-
Related to fold functions
=========================
-}

import Data.List
  ( find,
    lookup,
  )
import Prelude hiding
  ( foldl1,
    foldr1,
    iterate,
    scanl,
    scanr,
  )

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs)
foldr1 _ [] = error "foldr1: EmptyList"

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs
foldl1 _ [] = error "foldl1: EmptyList"

maximum :: (Ord a) => [a] -> a
maximum = foldl1 max

{-
TASK
====
Напишите реализацию функции, возвращающей последний элемент списка, через
`foldl1`.

lastElem :: [a] -> a
lastElem = foldl1 undefined

SOLUTION
========
-}
lastElem :: [a] -> a
lastElem = foldr1 $ const id

-- >>> lastElem [1, 2] == 2
-- True
--------------------------------------------------------------------------------

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ ini [] = [ini]
scanl f ini (x : xs) = ini : scanl f (ini `f` x) xs

-- >>> scanl (*) 1 [1..10]
-- [1,1,2,6,24,120,720,5040,40320,362880,3628800]

facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1 ..]

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

-- >>> facs !! 5
-- >>> facs !! 50
-- 120
-- 30414093201713378043612608166064768844377641568960512000000000000

-- >>> partialSums [1..10]
-- [0,1,3,6,10,15,21,28,36,45,55]

-- NOTE: "^" for positive powers, "**" for negative ones
-- >>> take 15 . partialSums . map (**(-1)) $ facs
-- [0.0,1.0,2.0,2.5,2.6666666666666665,2.708333333333333,2.7166666666666663,2.7180555555555554,2.7182539682539684,2.71827876984127,2.7182815255731922,2.7182818011463845,2.718281826198493,2.7182818282861687,2.7182818284467594]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ ini [] = [ini]
scanr f ini (x : xs) = (x `f` q) : qs
  where
    qs@(q : _) = scanr f ini xs

-- >>> scanr (+) 0 []
-- >>> scanr (+) 0 [1, 2, 3]
-- [0]
-- [6,5,3,0]

unfold :: (b -> (a, b)) -> b -> [a]
unfold f ini =
  let (x, ini') = f ini
   in x : unfold f ini'

iterate f = unfold (\x -> (x, f x))

-- >>> take 10 . iterate (^2) $ 2
-- [2,4,16,256,65536,4294967296,18446744073709551616,340282366920938463463374607431768211456,115792089237316195423570985008687907853269984665640564039457584007913129639936,13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096]

-- > :t Nothing
-- > Nothing :: Maybe a

-- > :t Just
-- > Just :: a -> Maybe a

-- > :t Just True
-- > Just True :: Maybe Bool

-- > :t Just False
-- > Just False :: Maybe Bool

-- > :t find
-- > find :: (a -> Bool) -> [a] -> Maybe a

-- >>> find odd [2, 3, 4]
-- >>> find odd [2, 6, 4]
-- Just 3
-- Nothing

-- > :t lookup
-- > lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- >>> lookup 2 [(2, 'a'), (3, 'b')]
-- >>> lookup 4 [(2, 'a'), (3, 'b')]
-- Just 'a'
-- Nothing

-- NOTE: `unfold` in `Data.List` module
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini)
  where
    helper (Just (x, ini')) = x : unfoldr f ini'
    helper Nothing = []

-- >>> unfoldr (\x -> if x == 10 then Nothing else Just(x, x + 2)) 0
-- [0,2,4,6,8]

{-
TASK
====
Используя `unfoldr`, реализуйте функцию, которая возвращает в обратном
алфавитном порядке список символов, попадающих в заданный парой диапазон.
Попадание символа `x` в диапазон пары `(a,b)` означает, `что x >= a и x <= b`.

GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"

SOLUTION
========
-}
revRange :: (Char, Char) -> [Char]
revRange (a, b) = unfoldr g b
  where
    g x
      | a <= x && x <= b = Just (x, pred x)
      | otherwise = Nothing

-- >>> revRange ('a','z')
-- "zyxwvutsrqponmlkjihgfedcba"
--------------------------------------------------------------------------------
