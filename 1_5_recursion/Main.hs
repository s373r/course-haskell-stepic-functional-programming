{-
Recursion
=========
-}

factorial n = if n == 0 then 1 else n * factorial (n - 1)

-- >>> factorial 4
-- 24

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- >>> factorial' 4
-- 24

{-
TASK
====
Определите функцию, вычисляющую двойной факториал, то есть произведение
натуральных чисел, не превосходящих заданного числа и имеющих ту же четность.

Например:
  7!!=7⋅5⋅3⋅1
  8!!=8⋅6⋅4⋅2

Предполагается, что аргумент функции может принимать только неотрицательные
значения.

SOLUTION
========
-}
doubleFact :: Integer -> Integer
doubleFact (-1) = 1
doubleFact 0 = 1
doubleFact n = n * doubleFact (n - 2)

-- >>> doubleFact 7 == 7 * 5 * 3 * 1
-- >>> doubleFact 8 == 8 * 6 * 4 * 2
-- True
-- True
--------------------------------------------------------------------------------

factorial'' 0 = 1
factorial'' n =
  if n < 0
    then error "Unexpected a negative argument!"
    else n * factorial'' (n - 1)

-- >>> factorial'' (-3)
-- Unexpected a negative argument!

{-
Guards
------
-}

factorial''' 0 = 1
factorial''' n
  | n < 0 = error "Unexpected a negative argument!"
  | n > 0 = n * factorial''' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n
  | n == 0 = 1
  | n > 0 = n * factorial4 (n - 1)
  | otherwise = error "Unexpected a negative argument!"

{-
TASK
====
В последнем примере предыдущего шага в охранном выражении использовался
идентификатор otherwise. Это не ключевое слово, а константа,
определенная для удобства в стандартной библиотеке:

otherwise = ?

Как вы думаете, какова правая часть её определения?

SOLUTION
========
True
-}

{-
TASK
====
Последовательность чисел Фибоначчи 0,1,1,2,3,5,8,13,21, …  легко определить
рекурсивно, задав два первых терминирующих значения и определив любое
последующее как сумму двух непосредственно предыдущих:

F_0 = 0
F_1 = 1
F_n = F_{n - 1} + F_{n - 2}

На Haskell данное определение задаётся следующей функцией:

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

Эта функция определена лишь для неотрицательных чисел. Однако, из данного выше
определения можно вывести формулу для вычисления чисел Фибоначчи
при отрицательных индексах, при этом последовательность будет следующей:

F_-1 = 1, F_-2 = -1, …, F_-10 = -55, …

Измените определение функции fibonacci так, чтобы она была определена
для всех целых чисел и порождала при отрицательных аргументах указанную
последовательность.

SOLUTION
========
-}
fibonacciOnlyPositive :: Integer -> Integer
fibonacciOnlyPositive n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacciOnlyPositive (n - 1) + fibonacciOnlyPositive (n - 2)

negativeSignForEven :: Integer -> Integer
negativeSignForEven v
  | even v = -1
  | otherwise = 1

fibonacci :: Integer -> Integer
fibonacci n = negativeSignForEven n * fibonacciOnlyPositive (abs n)

-- >>> fibonacci (-10) == (-55)
-- >>> fibonacci (-9)  == 34
-- True
-- True
--------------------------------------------------------------------------------

factorial5 n
  | n >= 0 = helper 1 n
  | otherwise = error "Unexpected a negative argument!"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

-- >>> factorial5 3
-- 6

{-
TASK
====
Реализация функции для вычисления числа Фибоначчи, основанная на прямом
рекурсивном определении, крайне неэффективна - количество вызовов функции растет
экспоненциально с ростом значения аргумента. GHCi позволяет отслеживать
использование памяти и затраты времени на вычисление выражения, для этого
следует выполнить команду :set +s:

GHCi> :set +s
GHCi> fibonacci 30
832040
(8.36 secs, 298293400 bytes)

С помощью механизма аккумуляторов попробуйте написать более эффективную
реализацию, имеющую линейную сложность (по числу рекурсивных вызовов). Как и в
предыдущем задании, функция должна быть определена для всех целых чисел.

SOLUTION
========
-}
fibonacciAcc :: Integer -> Integer -> Integer -> Integer
fibonacciAcc prev acc n
  | n == 0 = acc
  | otherwise = fibonacciAcc acc (acc + prev) (n - 1)

negativeSignForNegativeEven :: Integer -> Integer
negativeSignForNegativeEven v
  | v < 0 && even v = -1
  | otherwise = 1

fibonacci' :: Integer -> Integer
fibonacci' n = negativeSignForNegativeEven n * fibonacciAcc 1 0 (abs n)

-- >>> fibonacci' 30 == 832040
-- True
--------------------------------------------------------------------------------
