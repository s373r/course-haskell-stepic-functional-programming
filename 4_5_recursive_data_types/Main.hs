{-
Recursive data types
====================
-}

data List a = Nil | Cons a (List a)
  deriving (Show)

-- >>> :t Nil
-- Nil :: List a

-- >>> :t Cons 'z' Nil
-- Cons 'z' Nil :: List Char

yz = Cons 'y' (Cons 'z' Nil)

-- >>> yz
-- Cons 'y' (Cons 'z' Nil)

{-
NOTE: pseudo-code of standard List
data [] a = [] | a : ([] a)
-}

{-
TASK
====
Тип `List`, определенный ниже, эквивалентен определению списков из стандартной
библиотеки в том смысле, что существуют взаимно обратные функции, преобразующие
`List a` в `[a]` и обратно. Реализуйте эти функции.

SOLUTION
========
-}
-- NOTE: has declared above
-- data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil

--------------------------------------------------------------------------------

{-
TASK
====
Рассмотрим еще один пример рекурсивного типа данных:

```
data Nat = Zero | Suc Nat
```

Элементы этого типа имеют следующий вид: `Zero`, `Suc Zero`, `Suc (Suc Zero)`,
`Suc (Suc (Suc Zero))`, и так далее. Таким образом мы можем считать, что
элементы этого типа - это натуральные числа в унарной системе счисления.

Мы можем написать функцию, которая преобразует `Nat` в `Integer` следующим
образом:

```
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
```

Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.

SOLUTION
========
-}
data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add a b = toNat $ fromNat a + fromNat b

mul :: Nat -> Nat -> Nat
mul a b = toNat $ fromNat a * fromNat b

factorial :: Integer -> Integer
factorial n = product [1 .. n]

fac :: Nat -> Nat
fac = toNat . factorial . fromNat

--------------------------------------------------------------------------------

{-
TASK
====
Тип бинарных деревьев можно описать следующим образом:

```
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

Реализуйте функцию `height`, возвращающую высоту дерева, и функцию `size`,
возвращающую количество узлов в дереве (и внутренних, и листьев). Считается,
что дерево, состоящее из одного листа, имеет высоту `0`.

SOLUTION
========
-}
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- 1st variant
-- height :: Tree a -> Int
-- height = go 0
--   where
--     go :: Int -> Tree a -> Int
--     go n (Leaf _) = n
--     go n (Node a b) = max (go (n + 1) a) (go (n + 1) b)

-- 2nd variant
height :: Tree a -> Int
height (Leaf _) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = 1 + size a + size b

--

testTree1 :: Tree Integer
testTree1 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

--        testTree1
--        ---------
--            x
--           / \
--          1   x
--             / \
--            2   3

testTree2 :: Tree Integer
testTree2 = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Node (Leaf 2) (Leaf 3))

--        testTree2
--        ---------
--            x
--           / \
--          x   x
--         /|   |\
--        1 x   2 3
--         / \
--        2   3

-- >>> size testTree1 == 5
-- >>> size testTree2 == 9
-- >>> height testTree1 == 3
-- >>> height testTree2 == 4
-- True
-- True
-- True
-- True

-- >>> height (Leaf 1) == 0
-- >>> height (Node (Leaf 1) (Leaf 1)) == 1
-- >>> height (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) == 2
-- >>> height (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) == 2
-- >>> height (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) == 3
-- >>> height (Node (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Leaf 1) (Leaf 1))) == 3
-- >>> height (Node (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1))) == 3
-- >>> height (Node (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1)))) == 3
-- >>> height (Node (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1)))) == 4
-- >>> size (Leaf 1) == 1
-- >>> size (Node (Leaf 1) (Leaf 1)) == 3
-- >>> size (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) == 5
-- >>> size (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) == 7
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Теперь нам нужно написать функцию `avg`, которая считает среднее арифметическое
всех значений в дереве. И мы хотим, чтобы эта функция осуществляла только один
проход по дереву. Это можно сделать при помощи вспомогательной функции,
возвращающей количество листьев и сумму значений в них. Реализуйте эту функцию.

SOLUTION
========
-}

-- NOTE: has declared above
-- data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
  let (c, s) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf x) = (1, x)
    go (Node a b) = (a1 + b1, a2 + b2)
      where
        (a1, a2) = go a
        (b1, b2) = go b

--------------------------------------------------------------------------------

infixl 6 :+:

infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
  deriving (Show, Eq)

expr1 :: Expr
expr1 = Val 2 :+: Val 3 :*: Val 4

expr2 :: Expr
expr2 = (Val 2 :+: Val 3) :*: Val 4

-- >>> expr1
-- >>> expr2
-- Val 2 :+: Val 3 :*: Val 4
-- (Val 2 :+: Val 3) :*: Val 4

-- >>> Val 2 :+: Val 2 == Val 4
-- False

eval :: Expr -> Int
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

-- >>> eval expr1
-- >>> eval expr2
-- 14
-- 20

-- >>> eval (Val 2 :+: Val 2) == eval (Val 4)
-- True

expand' :: Expr -> Expr
expand' ((e1 :+: e2) :*: e) = expand' e1 :*: expand' e :+: expand' e2 :*: expand' e
expand' (e :*: (e1 :+: e2)) = expand' e :*: expand' e1 :+: expand' e :*: expand' e2
expand' (e1 :+: e2) = expand' e1 :+: expand' e2
expand' (e1 :*: e2) = expand' e1 :*: expand' e2
expand' e = e

-- >>> expr2
-- >>> expand' expr2
-- (Val 2 :+: Val 3) :*: Val 4
-- Val 2 :*: Val 4 :+: Val 3 :*: Val 4

{-
TASK
====
Исправьте определение функции `expand`

```
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e
```

так, чтобы она, используя дистрибутивность (а также, возможно, ассоциативность
и коммутативность), всегда возвращала значение, эквивалентное данному и
являющееся суммой произведений числовых значений. Например,

```
GHCi> expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))
```

Примечание. Скобки в ответе могут быть расставлены по-другому или вообще
отсутствовать, поскольку сложение ассоциативно. Слагаемые могут идти в другом
порядке, поскольку сложение коммутативно.

SOLUTION
========
-}
expand :: Expr -> Expr
expand e
  | e /= e' = expand e'
  | otherwise = e
  where
    e' = go e
    go :: Expr -> Expr
    go ((e1 :+: e2) :*: e) = go e1 :*: go e :+: go e2 :*: go e
    go (e :*: (e1 :+: e2)) = go e :*: go e1 :+: go e :*: go e2
    go (e1 :+: e2) = go e1 :+: go e2
    go (e1 :*: e2) = go e1 :*: go e2
    go e = e

-- (1 + 2 + 3) * (4 + 5) -> 1 * (4 + 5) + 2 * (4 + 5) + 3 * (4 + 5)
--                          ^^^^^^^^^^^   ^^^^^^^^^^^   ^^^^^^^^^^^
--                                    a             b             c
--                       -> 1*4 + 1*5 + 2 * (4 + 5) + 3 * (4 + 5)
--                                      ^^^^^^^^^^^   ^^^^^^^^^^^
--                                                b             c
--                       -> 1*4 + 1*5 + 2*4 + 2*5 + 3 * (4 + 5)
--                                                  ^^^^^^^^^^^
--                                                            c
--                       -> 1*4 + 1*5 + 2*4 + 2*5 + 3*4 + 3*5
--
--              expected -> 1*4 + 1*5 + 2*4 + 2*5 + 3*4 + 3*5

-- >>> expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
-- ((Val 1 :*: Val 4 :+: Val 1 :*: Val 5) :+: (Val 2 :*: Val 4 :+: Val 2 :*: Val 5)) :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5)

-- -> ((1*4 + 1*5) + (2*4 + 2*5)) + (3*4 + 3*5)
-- -> (1*4 + 1*5) + (2*4 + 2*5) + (3*4 + 3*5)
-- -> 1*4 + 1*5 + (2*4 + 2*5) + (3*4 + 3*5)
-- -> 1*4 + 1*5 + 2*4 + 2*5 + (3*4 + 3*5)
-- -> 1*4 + 1*5 + 2*4 + 2*5 + 3*4 + 3*5
-- OK!

--------------------------------------------------------------------------------
