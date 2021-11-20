{-
Local bindings and indentation rules
====================================
-}

-- NOTE: shown indentation rules reformatted with the code formatter
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
  ( (- b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a),
    (- b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  )

roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c)
   in ( (- b - d) / (2 * a),
        (- b + d) / (2 * a)
      )

-- >>> let x = True in (True, x)
-- (True,True)

roots'' :: Double -> Double -> Double -> (Double, Double)
roots'' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c); x1 = (- b - d) / (2 * a); x2 = (- b + d) / (2 * a)
   in (x1, x2)

roots''' :: Double -> Double -> Double -> (Double, Double)
roots''' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c)
      x1 = (- b - d) / aTwice
      x2 = (- b + d) / aTwice
      aTwice = 2 * a
   in (x1, x2)

{-
TASK
====
Не используя GHCi, определите строку, которая является значением выражения
(let x = 'w' in [x,'o',x]) ++ "!".

SOLUTION
========
wow!
-}

factorial6 n
  | n >= 0 =
    let helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
     in helper 1 n
  | otherwise = error "Unexpected a negative argument!"

rootsDiff a b c =
  let (x1, x2) = roots a b c
   in x2 - x1

{-
TASK
====
Реализуйте функцию seqA, находящую элементы следующей рекуррентной
последовательности

a_0 = 1; a_1 = 2 ; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} - 2 a_{k}.

Попытайтесь найти эффективное решение.

GHCi> seqA 301
1276538859311178639666612897162414

SOLUTION
========
seqA :: Integer -> Integer
seqA n =
  let f _ _ n1 0 = n1
      f n3 n2 n1 n = f (n3 + n2 - 2 * n1) n3 n2 (n - 1)
   in f 3 2 1 n
-}

roots'''' :: Double -> Double -> Double -> (Double, Double)
roots'''' a b c = (x1, x2)
  where
    d = sqrt (b ^ 2 - 4 * a * c)
    x1 = (- b - d) / aTwice
    x2 = (- b + d) / aTwice
    aTwice = 2 * a

{-
Difference between let in & where
---------------------------------
A result of `let in` is an expression
-}

-- >>> let x = 2 in x ^ 2
-- 4

-- >>> (let x = 2 in x ^ 2) ^ 2
-- 16

-- >>> x ^ 2 where x = 2
-- parse error on input ‘where’

-- With `where`, we can use declarations in ALL guard
-- Just in case, here we do not use this option
factorial7 n
  | n >= 0 = helper 1 n
  | otherwise = error "Unexpected a negative argument!"
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)

{-
TASK
====
Реализуйте функцию, находящую сумму и количество цифр десятичной записи
заданного целого числа.

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = undefined

GHCi> sum'n'count (-39)
(12,2)

SOLUTION
========
import Data.Char (digitToInt)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (toInteger (sum digits), toInteger (length digits))
  where
    digits = map digitToInt (show (abs x))
-}

{-
TASK
====
Реализуйте функцию, находящую значение определённого интеграла от заданной
функции f на заданном интервале [a,b] методом трапеций. (Используйте равномерную
сетку; достаточно 1000 элементарных отрезков.)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined

GHCi> integration sin pi 0
-2.0

Результат может отличаться от -2.0, но не более чем на 1e-4.

SOLUTION
========
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = trapezoidalIntegral 1000
  where
    trapezoidalIntegral n = h * (0.5 * (f a + f b) + sum 0 (n - 1))
      where
        h = (b - a) / n
        sum acc i
          | i == 0 = acc
          | otherwise = sum (acc + f (x i)) (i - 1)
          where
            x i = a + i * h
-}
