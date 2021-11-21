{-
Lax semantics
=============
-}

sumIt :: Int -> Int -> Int
sumIt x y = x + y

-- >>> sumIt (1 + 2) 3
-- 6

{-
Lazy evaluation (used in Haskell)
---------------------------------
A reducible expression (redex)

redex
|||||  redex
↓↓↓↓↓  ↓↓↓↓↓
sumIt (2 + 3)   4
      (2 + 3) + 4
            5 + 4
                9
-}

{-
Eager (energetic) evaluation
----------------------------
sumIt (2 + 3)   4
sumIt (    5)   4
            5 + 4
                9
-}

{-
TASK
====
Предположим, что стандартные функции определены следующим образом:

> id x = x
> const x y = x
> max x y = if x <= y then y else x
> infixr 0 $
> f $ x = f x

Сколько редексов имеется в следующем выражении

const $ const (4 + 5) $ max 42

Примечание. Мы определили шаг вычислений как подстановку тела функции вместо
ее имени с заменой всех ее формальных параметров на фактически переданные ей
выражения. Редексом при этом мы называем подвыражение, над которым можно
осуществить подобный шаг.

SOLUTION
========
3
-}

add7 :: Int -> Int -> Int
add7 x y = x + 7

{-
Lazy
----
        uncalculated
        ↓↓↓↓↓
add7 1 (2 + 3)
     1         + 7
                 8
-}

{-
Eager
-----
add7 1 (2 + 3)
add7 1      5  <-- an extra step
     1        + 7
                8
-}

dup :: Int -> (Int, Int)
dup x = (x, x)

{-
Lazy
----
dup (2 + 3       )
    (2 + 3, 2 + 3)
    (    5, 2 + 3) <-- an extra step
    (    5,     5)

dup (2 + 3       )
    (    p,     p) p = 2 + 3 = 5
    (    5,     5)
-}

{-
Eager
-----
dup (2 + 3)
dup (    5)
    ( 5, 5)
-}

{-
TASK
====
Сколько шагов редукции потребуется, чтобы вычислить значение функции `value`,
если используется ленивая стратегия вычислений с механизмом разделения?

> bar x y z = x + y
> foo a b = bar a a (a + b)
> value = foo (3 * 10) (5 - 2)

Примечание. Подстановку тела функции `value` вместо `value` не считайте.

SOLUTION
========
4
-}

const42 :: a -> Int
const42 = const 42

-- >>> const42 True
-- >>> const42 123
-- >>> const42 (1 + 3)
-- >>> const42 undefined
-- 42
-- 42
-- 42
-- 42

{-
TASK
====
Отметьте функции, которые не могут привести к расходимости ни на каком корректном наборе аргументов.

> foo a = a
>
> bar = const foo
>
> baz x = const True
>
> quux = let x = x in x
>
> corge = "Sorry, my value was changed"
>
> grault x 0 = x
> grault x y = x
>
> garply = grault 'q'
>
> waldo = foo

SOLUTION
========
[ ] waldo
[ ] quux
[x] baz
[ ] garply
[ ] grault
[ ] bar
[x] corge
[ ] foo
-}

{-
Normal form (NF)
----------------
42
(3, 4)
\x -> x + 2
-}

{-
Not normal form
---------------
"Real" ++ "world"
sin (pi / 2)
(\x -> x + 2) 5
(3, 1 + 5)
-}

{-
Weak head normal form (WHNM)
----------------------------
\x -> x + 2 * 3
(3, 1 + 5)
(, 4 * 5) or (,) (4 * 5)
(+) (7 ^ 2)
-}

{-
TASK
====
Какие из выражений ниже не находятся в нормальной форме, но находятся в слабой
головной нормальной форме?

SOLUTION
========
[ ] fst (1,0)
[ ] 3
[x] [undefined, 4 + 5, -1]
[x] (,) undefined
[x] (+) (2 * 3 * 4)
[ ] \x -> x
-}

{-
`seq` forces evaluation to WHNM
-}

-- seq :: a -> b -> b
-- seq _|_ b = _|_
-- seq a b = b

-- >>> seq 1 2
-- 2

-- >>> seq undefined 2
-- Prelude.undefined

-- >>> seq (id undefined) 2
-- Prelude.undefined

-- >>> seq (undefined, undefined) 2
-- 2

-- >>> seq (\x -> undefined) 2
-- 2

{-
TASK
====

При вычислении каких из перечисленных ниже функций использование `seq`
предотвратит нарастание количества невычисленных редексов при увеличении
значения первого аргумента:

> foo 0 x = x
> foo n x = let x' = foo (n - 1) (x + 1)
>           in x' `seq` x'
>
> bar 0 f = f
> bar x f = let f' = \a -> f (x + a)
>               x' = x - 1
>          in f' `seq` x' `seq` bar x' f'
>
> baz 0 (x, y) = x + y
> baz n (x, y) = let x' = x + 1
>                    y' = y - 1
>                    p  = (x', y')
>                    n' = n - 1
>                in p `seq` n' `seq` baz n' p
>
> quux 0 (x, y) = x + y
> quux n (x, y) = let x' = x + 1
>                     y' = y - 1
>                     p  = (x', y')
>                     n' = n - 1
>                 in x' `seq` y' `seq` n' `seq` quux n' p

SOLUTION
========
[ ] foo
[ ] bar
[ ] baz
[x] quux
-}

{-
($!) :: (a -> b) -> a -> b
f $! x = x `seq f x
         ↑        ↑
         │        already evaluated
         ╰─────── here
-}

-- >>> const 42 undefined
-- >>> const 42 $ undefined
-- >>> const 42 $! undefined
-- 42
-- 42
-- Prelude.undefined

factorial :: Integer -> Integer
factorial n
  | n >= 0 = helper 1 n
  | otherwise = error "arg must be >= 0"
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n -1)

factorialStrict :: Integer -> Integer
factorialStrict n
  | n >= 0 = helper 1 n
  | otherwise = error "arg must be >= 0"
  where
    helper acc 0 = acc
    helper acc n = (helper $! (acc * n)) (n -1)

{-
TASK
====
Ниже определены функции `mySum` и `goSum`. Вызов `goSum` может выглядеть,
к примеру, так:  `goSum 15`. Выберите верные утверждения, описывающие процесс
вычисления подобного выражения.

> mySum acc 0 = acc
> mySum (result, ()) n = (mySum $! (result + n, ())) $ n - 1
>
> goSum = mySum (0, ())

SOLUTION
========
[ ] В первом аргументе функции `mySum` не будут накапливаться отложенные
    вычисления, так как при рекурсивных вызовах используется оператор $!.
[ ] В первом аргументе функции `mySum` не будут накапливаться отложенные
    вычисления, так как он будет находиться в слабой головной нормальной форме.
[x] В первом аргументе функции `mySum` будут накапливаться отложенные
    вычисления.
[ ] Во втором аргументе функции `mySum` будут накапливаться отложенные
    вычисления из-за того, что его передача при рекурсивном вызове происходит с помощью
    оператора $ а не $!.
[x] Во втором аргументе функции `mySum` не будут накапливаться отложенные
    вычисления, так как при каждом рекурсивном вызове происходит сопоставление
    с 0.
[ ] Во втором аргументе функции `mySum` не будут накапливаться отложенные
    вычисления, так как минус - примитивная операция.
-}
