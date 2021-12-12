{-
Enumeration types
=================
-}

import Prelude hiding
  ( Bool,
    False,
    True,
  )

-- NOTE: starts from capital letters
data Bool = True | False

-- >>> data b = T | F
-- Malformed head of type or class declaration: b

-- >>> data B = t | f
-- Not a data constructor: ‘t’

-- >>> data B = T | F

alwaysTrue :: Int -> Bool
alwaysTrue n = True

{-
TASK
====
Выберите корректные определения типов данных.

SOLUTION
========
[ ] data myType = A | B | C
[x] data Data = A | B
[x] data T = Con1 | Con2
[x] data T = A
[x] data Type = A | B | C
[ ] data T = Con1 | myCon2
-}
--------------------------------------------------------------------------------

data B = T | F
  deriving
    ( Show,
      Eq,
      Read,
      Enum
    )

not' :: B -> B
not' T = F
not' F = T

-- NOTE: result without `deriving Show`
-- >>> not' F
-- No instance for (Show B) arising from a use of ‘evalPrint’

-- >>> not' F
-- T

-- >>> F == T
-- False

-- >>> succ F
-- succ{B}: tried to take `succ' of last tag in enumeration

-- >>> succ T
-- F

not'' :: B -> B
not'' T = F

-- not'' F = T

-- >>> not'' T
-- F

-- >>> not'' F
-- Non-exhaustive patterns in function not''

-- NOTE: enable a warning for GHCi
-- > :set -fwarn-incomplite-patterns

{-
TASK
====
Тип данных `Color` определен следующим образом

```
data Color = Red | Green | Blue
```

Определите экземпляр класса `Show` для типа `Color`, сопоставляющий каждому
из трех цветов его текстовое представление.

GHCi> show Red
"Red"

SOLUTION
========
-}
-- NOTE: can be solved via deriving as well
-- data Color = Red | Green | Blue deriving (Show)

data Color = Red | Green | Blue deriving (Eq)

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

-- >>> show Red == "Red"
-- True
--------------------------------------------------------------------------------

intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
intToChar 4 = '4'
intToChar 5 = '5'
intToChar 6 = '6'
intToChar 7 = '7'
intToChar 8 = '8'
intToChar 9 = '9'
intToChar _ = 'N'

isz :: Char -> Bool
isz 'z' = True
isz _ = False

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False

{-
TASK
====
Определите частичную (определенную на значениях от `'0'` до `'9'`) функцию
`charToInt`.

```
GHCi> charToInt '0'
0
GHCi> charToInt '9'
9
```

SOLUTION
========
-}
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

-- >>> charToInt '0' == 0
-- >>> charToInt '9' == 9
-- True
-- True
--------------------------------------------------------------------------------

{-
TASK
====
Определите (частичную) функцию `stringToColor`, которая по строковому
представлению цвета как в прошлой задаче возвращает исходный цвет.

```
GHCi> stringToColor "Red"
Red
```

SOLUTION
========
-}
stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

-- >>> stringToColor "Red" == Red
-- True
--------------------------------------------------------------------------------

-- NOTE: pattern matching is lazy
--       it works from left to right & from top to bottom
foo 1 2 = 3
foo 0 _ = 5

-- >>> foo 0 undefined
-- >>> foo undefined 0
-- >>> foo 2 2
-- >>> foo 1 (5 - 3)
-- 5
-- Prelude.undefined
-- Non-exhaustive patterns in function foo
-- 3

bar (1, 2) = 3
bar (0, _) = 5

{-
TASK
====
Пусть определены следующие функции:

```
emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'
```

Выберите варианты вызовов этих функций, при которых сопоставление с образцом
будет осуществлено успешно.

SOLUTION
========
[ ] emptyOrSingleton undefined 5
[x] isEqual (undefined, undefined) (undefined, undefined)
[ ] isEqual undefined undefined
[x] emptyOrSingleton True undefined
[x] emptyOrSingleton False undefined
[ ] isEqual undefined (undefined, undefined)
[ ] isEqual (undefined, undefined) undefined
-}
--------------------------------------------------------------------------------

{-
TASK
====
Тип `LogLevel` описывает различные уровни логирования.

```
data LogLevel = Error | Warning | Info
```

Определите функцию `cmp`, сравнивающую элементы типа `LogLevel` так, чтобы
было верно, что `Error` > `Warning` > `Info`.

```
GHCi> cmp Error Warning
GT
GHCi> cmp Info Warning
LT
GHCi> cmp Warning Warning
EQ
```

SOLUTION
========
-}
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp Info _ = LT
cmp Warning Error = LT
cmp Warning Info = GT

-- >>> cmp Error Warning == GT
-- >>> cmp Info Warning == LT
-- >>> cmp Warning Warning == EQ
-- True
-- True
-- True
--------------------------------------------------------------------------------

-- NOTE: all pattern matching stuff are translated to `case of` at low-level
lessThatError :: LogLevel -> Bool
lessThatError lvl =
  case cmp lvl Error of
    LT -> True
    _ -> False

{-
TASK
====
Пусть объявлен следующий тип данных:

```
data Result = Fail | Success
```

И допустим определен некоторый тип данных `SomeData` и некоторая функция

```
doSomeWork :: SomeData -> (Result,Int)
```

возвращающая результат своей работы и либо код ошибки в случае неудачи,
либо `0` в случае успеха.

Определите функцию `processData`, которая вызывает `doSomeWork` и возвращает
строку `"Success"` в случае ее успешного завершения, либо строку `"Fail: N"`
в случае неудачи, где `N` — код ошибки.

SOLUTION
========
-}
data Result = Fail | Success

data SomeData

doSomeWork :: SomeData -> (Result, Int)
doSomeWork = undefined

processData :: SomeData -> String
processData x = case doSomeWork x of
  (Success, _) -> "Success"
  (_, n) -> "Fail: " ++ show n

--------------------------------------------------------------------------------
