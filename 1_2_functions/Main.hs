{-
Reductions
----------
(5 + 4 * 3) ^ 2
(5 +    12) ^ 2
(       17) ^ 2
(       17) ^ 2
---------------
            289
-}
-- >>> (5 + 4 * 3) ^ 2
-- 289

{-
A function call
---------------
Haskell: func firstArgument
  C/C++: func(firstArgument)
-}

-- >>> acos (cos pi)
-- 3.141592653589793

-- >>> max 5 42
-- 42

{-
Операция применения функции ассоциативна влево /
Function application is left associative
------------------------------------------------
-}
-- >>> (max 5) 42
-- 42

-- >>> 3 + sin 42
-- 2.0834784520843663

{-
Частичное применение / Partial application
------------------------------------------

"(max 5)" returns a function that takes one argument
-}
-- >>> 3 + (max 5) 42
-- 45

{-
TASK
====
В стандартной библиотеке Haskell есть функция вычисления логарифма
по произвольному основанию logBase. Это функция двух переменных, которой
требуется передать основание логарифма и аргумент, на котором логарифм будет
вычислен. Какие из следующих вызовов обеспечат вычисление логарифма
по основанию 2 от 8?

SOLUTION
========
[ ] logBase (2 8)
[x] logBase 2 8
[ ] logBase (2, 8)
[x] (logBase 2) 8
[ ] (logBase, 2, 8)
-}

{-
Function declaration
--------------------
-}

sumSquares x y = x ^ 2 + y ^ 2

-- >>> sumSquares 1 2
-- 5

-- ' symbol is allowed for function names
rock'n'rol = 42

{-
TASK
====
Реализуйте функцию трех аргументов lenVec3, которая вычисляет длину трехмерного
вектора. Аргументы функции задают декартовы координаты конца вектора, его начало
подразумевается находящимся в начале координат. Для извлечения квадратного корня
воспользуйтесь функцией sqrt, определенной в стандартной библиотеке.

GHCi> lenVec3 2 3 6
7.0

SOLUTION
========
-}
lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- >>> lenVec3 2 3 6 == 7.0
-- True
--------------------------------------------------------------------------------

{-
Constant function
-----------------
is a function without arguments
-}
fortyTwo = 39 + 3

-- >>> fortyTwo
-- 42

{-
Conditional expression
----------------------
Both branches (then/else) should be:
- present
- return same type
-}

f x = if x > 0 then 1 else (-1)

-- >>> f 5
-- 1

-- >>> f (-5)
-- -1

g x = (if x > 0 then 1 else (-1)) + 3

-- >>> g 5
-- 4

-- >>> g (-7)
-- 2

{-
TASK
====
Напишите реализацию функции sign, которая возвращает 1, если ей передано
положительное число, (-1), если отрицательное, и 0 в случае, когда передан 0.

GHCi> sign (-100)
-1
SOLUTION
========
-}
sign x = if x > 0 then 1 else (if x == 0 then 0 else (-1))

-- >>> sign (-100) == (-1)
-- True
--------------------------------------------------------------------------------

{-
Partial application
-}

max5 x = max 5 x

-- >>> max5 4
-- 5

-- >>> max5 42
-- 42

{-
Point-free style / Бесточечный стиль
------------------------------------
-}

max5' = max 5

-- >>> max5' 4
-- 5

-- >>> max5' 42
-- 42

dicount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standartDiscount = dicount 1000 5

-- >>> standartDiscount 2000
-- 1900.0

-- >>> standartDiscount 900
-- 900.0

{-
TASK
====
Предположим, мы разрабатываем на Haskell интерфейс системы перевода
для естественных языков. Он должен содержать функцию translate с параметрами
text, languageFrom и languageTo. Расположите параметры в таком порядке,
чтобы было удобно реализовывать следующие функции:
translateFromSpanishToRussian, translateFromEnglishToRussian и
translateToRussian.

SOLUTION
========
( ) translate languageFrom languageTo text
( ) translate languageFrom text languageTo
( ) translate text languageTo languageFrom
( ) translate text languageFrom languageTo
( ) translate languageTo text languageFrom
(x) translate languageTo languageFrom text
-}
