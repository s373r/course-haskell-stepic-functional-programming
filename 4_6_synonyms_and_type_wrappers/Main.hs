{-
Synonyms and type wrappers
==========================
-}

import Data.Char (isUpper)
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Prelude hiding (Monoid, lookup, mappend, mconcat, mempty)

-- type String = [Char]

allUpper :: String -> Bool
allUpper = all isUpper

type IntegerList = [Integer]

sumSquares :: IntegerList -> Integer
sumSquares = foldr1 (+) . map (^ 2)

xs :: IntegerList
xs = [1, 2]

ys :: [Integer]
ys = [1, 2]

-- >>> xs == ys
-- True

type AssosList k v = [(k, v)]

lookup' :: Eq k => k -> AssosList k v -> Maybe v
lookup' _ [] = Nothing
lookup' key ((x, y) : xys)
  | key == x = Just y
  | otherwise = lookup' key xys

type IntMap = Map.Map Int

-- >>> :k Map.Map
-- >>> :k IntMap
-- Map.Map :: * -> * -> *
-- IntMap :: * -> *

{-
TASK
====
Пусть синоним типа `Endo` определен следующим образом:

```
type Endo a = a -> a
```

Выберите из списка типы, эквивалентные `Endo (Endo Int)`.

SOLUTION
========
[ ] Int -> (Int -> Int)
[ ] Int -> Int -> Int -> Int
[ ] (Int -> Int) -> Int
[ ] Int
[x] (Int -> Int) -> (Int -> Int)
[ ] Int -> Int -> Int
[ ] Int -> Int -> (Int -> Int)
[ ] Int -> Int
[x] (Int -> Int) -> Int -> Int
-}

--------------------------------------------------------------------------------

newtype IntList = IList [Int]

example :: IntList
example = IList [1, 2]

-- >>> example
-- No instance for (Show IntList) arising from a use of ‘evalPrint’

newtype IntList' = IList' [Int]
  deriving (Show)

example' :: IntList'
example' = IList' [1, 2]

-- >>> example'
-- IList' [1,2]

-- NOTE: the same as IntList' but less effective and more strict
data IntList'' = IList'' [Int]
  deriving (Show)

ignore' :: IntList'' -> String
ignore' (IList'' _) = "Hello"

ignore :: IntList' -> String
ignore (IList' _) = "Hello"

-- >>> ignore' undefined
-- >>> ignore undefined
-- Prelude.undefined
-- "Hello"

-- NOTE: `Identity` is just a box for `a` in design time;
--       in runtime we store only `a`
newtype Identity a = Identity {runIdentity :: a}
  deriving (Eq, Ord)

-- >>> :k Identity
-- Identity :: * -> *

-- >>> :t Identity
-- Identity :: a -> Identity a

-- >>> :t runIdentity
-- runIdentity :: Identity a -> a

{-
TASK
====
Выберите корректные объявления типов.

SOLUTION
========
[x] newtype A a b = A a
[ ] newtype A = A A
[ ] newtype A a b = A a b
[ ] newtype A a = A
[ ] newtype A a = A a a
[x] newtype A a b = A b
[x] newtype A a = A a
[ ] newtype A = A A A
[ ] newtype A = A
[ ] newtype A = A a
-}

--------------------------------------------------------------------------------

-- NOTE: A monoid in mathematics is a semigroup with a binary associative
--       operation [1] and an identity element (e) [2]
--
--       e x = x = x e, x ∈ M
--
--       [1] Commonly referred to as "multiplication"
--       [2] Commonly referred to as "1"
class Monoid a where
  -- NOTE: identity (neutral) element
  mempty :: a
  mappend :: a -> a -> a

  -- NOTE: binary associative operation
  mconcat :: [a] -> a -- fold
  mconcat = foldr mappend mempty

{-
                           Laws
                           ----

         mempty `mappend` x === x
         x `mappend` mempty === x
(x `mappend` y) `mappend` z === x `mappend` (y `mappend` z)

                          Example
                          -------
mappend = (+)
mempty  = 0
                      0 + x === x
                      x + 0 === x
                (x + y) + z === x + (y + z)
-}

instance Monoid [a] where
  mempty = []
  mappend = (++)

-- >>> mconcat [[1, 2], [], [3, 4, 5]]
-- [1,2,3,4,5]

newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

-- >>> Sum 2 `mappend` Sum 3
-- Sum {getSum = 5}

newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

-- >>> Product 2 `mappend` Product 3
-- Product {getProduct = 6}

{-
TASK
====
Реализуйте представителя класса типов `Monoid` для типа `Xor`, в котором
`mappend` выполняет операцию `xor`.

SOLUTION
========
-}
newtype Xor = Xor {getXor :: Bool}
  deriving (Eq, Show)

instance Monoid Xor where
  mempty = Xor False
  a `mappend` b = Xor $ a /= b

--------------------------------------------------------------------------------

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (x1, x2) `mappend` (y1, y2) = (x1 `mappend` y1, x2 `mappend` y2)

-- >>> ("ABC", Product 2) `mappend` ("CDE", Product 3)
-- ("ABCCDE",Product {getProduct = 6})

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing

  -- NOTE: get the first non-empty element
  First Nothing `mappend` r = r
  l `mappend` _ = l

-- >>> mconcat [First Nothing, First (Just 3), First (Just 5)]
-- First {getFirst = Just 3}

-- >>> mconcat $ map First [Nothing, (Just 3), (Just 5)]
-- >>> (mconcat . map First) [Nothing, (Just 3), (Just 5)]
-- >>> [Nothing, (Just 3), (Just 5)] & map First & mconcat
-- First {getFirst = Just 3}
-- First {getFirst = Just 3}
-- First {getFirst = Just 3}

firstConcat :: [Maybe a] -> Maybe a
firstConcat = getFirst . mconcat . map First

firstConcat' :: [Maybe a] -> Maybe a
firstConcat' xs =
  xs
    & map First
    & mconcat
    & getFirst

-- >>> getFirst $ mconcat $ map First [Nothing, (Just 3), (Just 5)]
-- >>> [Nothing, (Just 3), (Just 5)] & map First & mconcat & getFirst
-- >>> firstConcat [Nothing, (Just 3), (Just 5)]
-- >>> firstConcat' [Nothing, (Just 3), (Just 5)]
-- Just 3
-- Just 3
-- Just 3
-- Just 3

{-
TASK
====
Реализуйте представителя класса типов `Monoid` для `Maybe'` a так, чтобы
`mempty` не был равен `Maybe' Nothing`. Нельзя накладывать никаких
дополнительных ограничений на тип `a`, кроме указанных в условии.

SOLUTION
========
-}

newtype Maybe' a = Maybe' {getMaybe :: Maybe a}
  deriving (Eq, Show)

instance (Monoid a) => Monoid (Maybe' a) where
  mempty = Maybe' $ Just mempty
  (Maybe' (Just a)) `mappend` (Maybe' (Just b)) = Maybe' (Just (a `mappend` b))
  _ `mappend` _ = Maybe' Nothing

-- >>> Maybe' (Just "zxc") `mappend` Maybe' (Just "asd") == Maybe' {getMaybe = Just "zxcasd"}
-- >>> Maybe' (Just "zxc") `mappend` mempty == Maybe' {getMaybe = Just "zxc"}
-- >>> mempty `mappend` Maybe' (Just "sd") == Maybe' {getMaybe = Just "sd"}
-- >>> mconcat [Maybe' (Just "a"), mempty, Maybe' (Just "b"), Maybe' (Just "c"), mempty] == Maybe' {getMaybe = Just "abc"}
-- True
-- True
-- True
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Ниже приведено определение класса `MapLike` типов, похожих на тип `Map`.
Определите представителя `MapLike` для типа `ListMap`, определенного ниже как
список пар ключ-значение. Для каждого ключа должно храниться не больше одного
значения. Функция `insert` заменяет старое значение новым, если ключ уже
содержался в структуре.

SOLUTION
========
-}
-- NOTE: has imported at top
-- import Prelude hiding (lookup)
-- import qualified Data.List as L
-- import Data.Maybe (isNothing)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap {getListMap :: [(k, v)]}
  deriving (Eq, Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup k (ListMap m) = L.lookup k m
  insert k v m'@(ListMap m)
    | hasNotValue = (ListMap . insert') m
    | otherwise = (ListMap . insert' . getListMap) (delete k m')
    where
      hasNotValue = isNothing $ lookup k m'
      insert' = (:) (k, v)
  delete k (ListMap m) = (ListMap . filter ((/= k) . fst)) m

-- NOTE: Fluent-like function chaining
-- instance MapLike ListMap where
--   empty = ListMap []
--   lookup k (ListMap m) = L.lookup k m
--   insert k v m'@(ListMap m)
--     | hasNotValue =
--       m
--         & insert'
--         & ListMap
--     | otherwise =
--       delete k m'
--         & getListMap
--         & insert'
--         & ListMap
--     where
--       hasNotValue =
--         lookup k m'
--           & isNothing
--       insert' = (:) (k, v)
--   delete k (ListMap m) =
--     m
--       & filter ((/= k) . fst)
--       & ListMap

--------------------------------------------------------------------------------

-- >>> :t [(*2), (+5), (^2)]
-- [(*2), (+5), (^2)] :: Num a => [a -> a]

-- >>> zipWith ($) [(*2), (+5), (^2)] [1, 2, 3]
-- [2,7,9]

-- NOTE: Endo shorthand for endomorphism
newtype Endo a = Endo {appEndo :: a -> a}

instance Monoid (Endo a) where
  mempty = Endo id
  Endo f `mappend` Endo g = Endo (f . g)

-- >>> :t map Endo [(*2), (+5), (^2)]
-- map Endo [(*2), (+5), (^2)] :: Num a => [Endo a]

-- >>> :t mconcat $ map Endo [(*2), (+5), (^2)]
-- mconcat $ map Endo [(*2), (+5), (^2)] :: Num a => Endo a

-- >>> :t appEndo $ mconcat $ map Endo [(*2), (+5), (^2)]
-- appEndo $ mconcat $ map Endo [(*2), (+5), (^2)] :: Num a => a -> a

-- >>> (appEndo $ mconcat $ map Endo [(*2), (+5), (^2)]) 4
-- 42

-- ((4 ^ 2) + 5) * 2 = 42

{-
TASK
====
Реализуйте представителя `MapLike` для типа `ArrowMap`, определенного ниже.

SOLUTION
========
-}
-- NOTE: has imported above
-- import Prelude hiding (lookup)

-- NOTE: has declared above
-- class MapLike m where
--   empty :: m k v
--   lookup :: Ord k => k -> m k v -> Maybe v
--   insert :: Ord k => k -> v -> m k v -> m k v
--   delete :: Ord k => k -> m k v -> m k v
--   fromList :: Ord k => [(k, v)] -> m k v
--   fromList [] = empty
--   fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ArrowMap k v = ArrowMap {getArrowMap :: k -> Maybe v}

instance MapLike ArrowMap where
  empty = ArrowMap $ const Nothing

  lookup k (ArrowMap m) = m k

  insert k v (ArrowMap m) = ArrowMap f
    where
      f x =
        if x /= k
          then m x
          else Just v

  delete k (ArrowMap m) = ArrowMap f
    where
      f x =
        if x /= k
          then m x
          else Nothing

  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)

--------------------------------------------------------------------------------
