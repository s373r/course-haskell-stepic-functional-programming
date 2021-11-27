{-
Operators
=========
All operators are declared in the standard library
-}

{-
Prefix notation (or functional style) / Префиксный стиль (функциональный)
-------------------------------------------------------------------------
-}
-- >>> max 5 6
-- 6

-- >>> (+) 6 7
-- 13

{-
Infix notation (operator style) / Инфиксный стиль (операторный)
---------------------------------------------------------------
-}
-- >>> 6 + 7
-- 13

-- >>> 6 `max` 7
-- 7

{-
All operators* are binary, so 2 arguments should be passed

*expect minus operator
-}

{-
Minus operator variants
---------------
Binary: (-) 5 3
 Unary: (-7)
-}

{-
Operators priority
------------------
[1, 9] for operators
    10 for functions
-}

-- >>> 3 + 5 * 8
-- 43

-- >>> sin 5 + 4
-- 3.0410757253368614

{-
Operators associativity
-----------------------
Syntax: infix[lr] priority ...operators
  infixl - left associativity
  infixr - right associativity
  infix  - we cannot build a operator chain:
           a == b == c

Examples from the standard library:
  infixr 8 ^, `logBase`
  infixl 7 *, /, `div`, `mod`
  infixl 6 +, -
  infix 4 ==, /=, >, >=, <, <=

New operator haves "infixl 9" by default
-}

-- >>> 3 - 9 - 5
-- -11

{-
TASK
====
Попробуйте вычислить значение выражения 2 ^ 3 ^ 2, не используя GHCi.

SOLUTION
========
512
-}

{-
TASK
====
Попробуйте вычислить значение выражения (*) 2 ((+) 1 4) ^ 2, не используя GHCi.

SOLUTION
========
100
-}

{-
Custom operators
----------------
Can be build with [1, N] symbols

Symbols for operators:
! # $ % & * + . / < = > & @ \ ^ | - ~
and
: if used in the middle
-}

infixl 6 *+*

-- Infix notation
a *+* b = a ^ 2 + b ^ 2
-- Or prefix notation
(*+*) a b = a ^ 2 + b ^ 2

-- >>> 3 *+* 4
-- 25

-- >>> (*+*) 3 4
-- 25

{-
TASK
====
Используя данное выше определение оператора (*+*):

> infixl 6 *+*
> (*+*) a b = a ^ 2 + b ^ 2

попробуйте устно вычислить значение выражения 1 + 3 *+* 2 * 2.

SOLUTION
========
32
-}

{-
TASK
====
Реализуйте оператор |-|, который возвращает модуль разности переданных ему аргументов:

GHCi>  5 |-| 7
2

SOLUTION
========
-}
x |-| y = abs (x - y)

-- >>> 5 |-| 7 ==2
-- True
--------------------------------------------------------------------------------

{-
Partial application of operators
--------------------------------
-}
-- Сечение оператора / Operator section
-- >>> (2 /) 4
-- 0.5

-- >>> (/2) 4
-- 2.0

-- An exception: minus operator:
-- >>> (-2)
-- -2

{-
TASK
====
Попробуйте вычислить значение выражения (`mod` 14) ((+ 5) 10),
не используя GHCi. (Функция mod возвращает остаток от целочисленного деления
первого своего аргумента на второй.)

SOLUTION
========
1
-}

{-
Применение функции к аргументу / Applying a function to an argument
-------------------------------------------------------------------
-}

-- Already defined in the standard library
-- f $ x = f x

-- >>> sin 0
-- 0.0

-- >>> sin $ 0
-- 0.0

{-
Dollar operator
--------------
has 0 priority and right associativity

f (g x (h y)) ==
f $ g x (h y) ==
f $ g x $ h y
-}

-- >>> sin (pi / 2)
-- 1.0

-- >>> sin $ pi / 2
-- 1.0

{-
TASK
====
Используя оператор $, перепишите выражение logBase 4 (min 20 (9 + 7))
без скобок. (Разделяйте все токены одним пробелом.)

SOLUTION
========
-}
old = logBase 4 (min 20 (9 + 7))

new = logBase 4 $ min 20 $ 9 + 7

-- >>> old == new
-- True
--------------------------------------------------------------------------------
