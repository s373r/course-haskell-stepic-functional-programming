{-
Functor typeclass and laws for it
=================================
-}

import Data.Char (toUpper)
import Data.Functor
import Prelude hiding (Functor, fmap)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- >>> :k []
-- [] :: * -> *

-- instance Functor [] where
--   fmap = map

-- >>> map succ [1, 2, 3]
-- >>> fmap succ [1, 2, 3]
-- [2,3,4]
-- [2,3,4]

-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just a) = Just (f a)

-- >>> fmap (*2) Nothing
-- >>> fmap (*2) (Just 21)
-- Nothing
-- Just 42

{-
TASK
====
Определите представителя класса `Functor` для следующего типа данных,
представляющего точку в трёхмерном пространстве:

```
data Point3D a = Point3D a a a deriving Show
```

```
GHCi> fmap (+ 1) (Point3D 5 6 7)
Point3D 6 7 8
```

SOLUTION
========
-}
data Point3D a = Point3D a a a
  deriving (Show)

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

-- >>> show (fmap (+ 1) (Point3D 5 6 7)) == "Point3D 6 7 8"
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Определите представителя класса `Functor` для типа данных `GeomPrimitive`,
который определён следующим образом:

```
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
```

При определении, воспользуйтесь тем, что `Point3D` уже является представителем
класса Functor.

```
GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)
```

```
GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)
```

SOLUTION
========
-}
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
  -- NOTE: Deriving added for tests
  deriving (Show)

instance Functor GeomPrimitive where
  fmap f (Point a) = Point $ fmap f a
  fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)

-- >>> show (fmap (+ 1) $ Point (Point3D 0 0 0)) == "Point (Point3D 1 1 1)"
-- >>> show (fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)) == "LineSegment (Point3D 1 1 1) (Point3D 2 2 2)"
-- True
-- True

--------------------------------------------------------------------------------

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
  deriving (Show)

testTree = Branch (Leaf 2) 3 (Leaf 4)

-- >>> testTree
-- Branch (Leaf 2) 3 (Leaf 4)

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)

-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- >>> fmap (^2) testTree
-- >>> (^2) `fmap` testTree
-- >>> (^2) <$> testTree
-- Branch (Leaf 4) 9 (Leaf 16)
-- Branch (Leaf 4) 9 (Leaf 16)
-- Branch (Leaf 4) 9 (Leaf 16)

-- >>> (^2) <$> [1, 2, 3]
-- [1,4,9]

-- >>> :i <$>
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
--   	-- Defined in ‘Data.Functor’
-- infixl 4 <$>

-- >>> (+5) <$> (^2) <$> testTree
-- Branch (Leaf 9) 14 (Leaf 21)

-- >>> :i <$
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   ...
--   (<$) :: a -> f b -> f a
--   	-- Defined in ‘GHC.Base’
-- infixl 4 <$

-- >>> 42 <$ testTree
-- Branch (Leaf 42) 42 (Leaf 42)

{-
TASK
====
Определите представителя класса `Functor` для бинарного дерева, в каждом узле
которого хранятся элементы типа `Maybe`:

```
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
```

```
GHCi> words <$> Leaf Nothing
Leaf Nothing
```

```
GHCi> words <$> Leaf (Just "a b")
Leaf (Just ["a","b"])
```

SOLUTION
========
-}

-- NOTE: imported above
-- import Data.Functor

-- NOTE: Tree' is used since Tree has already declared

data Tree' a = Leaf' (Maybe a) | Branch' (Tree' a) (Maybe a) (Tree' a)
  deriving (Eq, Show)

instance Functor Tree' where
  fmap f (Leaf' x) = Leaf' (f <$> x)
  fmap f (Branch' l x r) = Branch' (f <$> l) (f <$> x) (f <$> r)

-- >>> (words <$> Leaf' Nothing) == Leaf' Nothing
-- >>> (words <$> Leaf' (Just "a b")) == Leaf' (Just ["a","b"])
-- True
-- True

--------------------------------------------------------------------------------

-- instance Functor ((,) s) where
--   fmap g (x, y) = (x, g y)

-- (a -> b) -> (s, a) -> (s, b)

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,) Int
-- (,) Int :: * -> *

-- >>> fmap succ (1, 'A')
-- (1,'B')

-- instance Functor (Either e) where
--   fmap _ (Left x) = Left x
--   fmap g (Right y) = Right (g x)

-- (a -> b) -> Either e a -> Either e b

-- >>> :k Either
-- Either :: * -> * -> *

-- >>> fmap (+3) $ Right 1
-- >>> fmap (+3) $ Left 1
-- >>> fmap (+3) $ Left "AAA"
-- Right 4
-- Left 1
-- Left "AAA"

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (->) Int
-- (->) Int :: * -> *

-- NOTE: e means environment

-- instance Functor ((->) e) where
--   fmap = (.)

-- (a -> b) -> (e -> a) -> (e -> b)

-- >>> :t fmap length tail
-- fmap length tail :: [a] -> Int

-- ([a] -> Int) -> ([a] -> [a]) -> ([a] -> Int)

-- >>> fmap length tail "ABC"
-- >>> length (tail "ABC")
-- >>> (length . tail) "ABC"
-- 2
-- 2
-- 2

{-
TASK
====
Определите представителя класса `Functor` для типов данных `Entry` и `Map`.
Тип `Map` представляет словарь, ключами которого являются пары:

```
data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show
```

В результате должно обеспечиваться следующее поведение: `fmap` применяет функцию
к значениям в словаре, не изменяя при этом ключи.

```
GHCi> fmap (map toUpper) $ Map []
Map []
```

```
GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]
```
SOLUTION
========
-}
data Entry k1 k2 v = Entry (k1, k2) v
  deriving (Eq, Show)

data Map k1 k2 v = Map [Entry k1 k2 v]
  deriving (Eq, Show)

instance Functor (Entry k1 k2) where
  fmap f (Entry k v) = Entry k (f v)

instance Functor (Map k1 k2) where
  fmap f (Map xs) = Map [fmap f x | x <- xs]

-- >>> (fmap (map toUpper) $ Map []) == Map []
-- >>> (fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]) == Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]
-- True
-- True

--------------------------------------------------------------------------------

{-
   Functor laws
   ------------
1. fmap id === id
2. fmap (f . g) === fmap f . fmap g
-}

-- 1.
-- >>> fmap id [1, 2 ,3]
-- >>> fmap id Just 42
-- [1,2,3]
-- Just 42

-- 2.
-- >>> (fmap (+1) . fmap (^2)) [1, 2, 3]
-- >>> fmap ((+1) . (^2)) [1, 2, 3]
-- [2,5,10]
-- [2,5,10]
