{-
Type classes
============
-}

-- > :t 7
-- 7 :: Num a => a

-- > :t (+)
-- (+) :: Num a => a -> a -> a

-- > :t (>)
-- (>) :: Ord a => a -> a -> Bool

-- > :t (> 7)
-- (> 7) :: (Num a, Ord a) => a -> Bool

-- > :t (> (1, 2))
-- (> (1,2)) :: (Num t, Num t1, Ord t, Ord t1) => (t, t1) -> Bool

-- > :t (* 'c')
-- No instance for (Num Char) arising from a use of `*`

{-
TASK
====
На нехватку какого представителя какого класса типов пожалуется интерпретатор
при попытке вывести тип выражения

> True + False * False

Запишите ответ в виде Имя_класса_типов Имя_типа. Постарайтесь ответить,
не используя GHCi.

SOLUTION
========
Num Bool
-}

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
-- NOTE:
--   (==), (/=) :: a -> a -> Bool

-- > :t (==)
-- (==) :: Eq a => a -> a -> Bool

-- > :t (== 42)
-- (== 42) :: (Eq a, Num a) => a -> Bool

-- > :t (== 'x')
-- (== 'x') :: Char -> Bool

-- > :t elem
-- elem :: Eq a => a -> [a] -> Bool

{-
TASK
====
Попробуйте, не используя GHCi или Hoogle, определить, какого контекста
не хватает в типе функции

> sort :: ? => [d] -> [d]

сортирующей переданный в нее список. Напишите выражение, которое должно стоять
на месте знака вопроса.

SOLUTION
========
Ord d
-}

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x /= y = not (x == y) -- NOTE: default implementations
--   x == y = not (x /= y) --       need to implement only one of them

-- instance Eq Bool where
--   True == True = True
--   False == False = True
--   _ == _ = False

{-
TASK
====
Реализуйте класс типов Printable, предоставляющий один метод toString — функцию
одной переменной, которая преобразует значение типа, являющегося представителем
Printable, в строковое представление.

Сделайте типы данных Bool и () представителями этого класса типов, обеспечив
следующее поведение:

GHCi> toString True
"true"
GHCi> toString False
"false"
GHCi> toString ()
"unit type"

SOLUTION
========
-}
class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString x = if x then "true" else "false"

instance Printable () where
  toString _ = "unit type"

-- >>> toString True  == "true"
-- >>> toString False == "false"
-- >>> toString ()    == "unit type"
-- True
-- True
-- True
--------------------------------------------------------------------------------

-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x /= y = not (x == y)
--   x == y = not (x /= y)

-- instance Eq Bool where
--   True == True = True
--   False == False = True
--   _ == _ = False

-- instance (Eq a, Eq b) => Eq (a, b) where
--   p1 == p2 = fst p1 == fst p2 && snd p1 == snd p2

-- Functions cannot be equal
-- >>> id == (\x -> x)
-- No instance for (Eq (a0 -> a0)) arising from a use of ‘==’
--   (maybe you haven't applied a function to enough arguments?)

{-
TASK
====
Сделайте тип пары представителем класса типов Printable, реализованного вами
в предыдущей задаче, обеспечив следующее поведение:

GHCi> toString (False,())
"(false,unit type)"
GHCi> toString (True,False)
"(true,false)"

Примечание. Объявление класса типов Printable и представителей этого класса
для типов () и  Bool заново реализовывать не надо — они присутствуют
в программе, вызывающей ваш код.

SOLUTION
========
-}
instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-- >>> toString (False, ())   == "(false,unit type)"
-- >>> toString (True, False) == "(true,false)"
-- True
-- True
--------------------------------------------------------------------------------
