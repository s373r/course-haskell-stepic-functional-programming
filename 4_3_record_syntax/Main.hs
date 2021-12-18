{-
Record syntax
=============
-}
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data Person' = Person' String String Int

firstName' :: Person' -> String
firstName' (Person' x _ _) = x

lastName' :: Person' -> String
lastName' (Person' _ y _) = y

age' :: Person' -> Int
age' (Person' _ _ z) = z

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Show, Eq)

jonh :: Person
jonh = Person "Jonh" "Smith" 33

-- >>> age jonh
-- 33

-- >>> jonh
-- Person {firstName = "Jonh", lastName = "Smith", age = 33}

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

-- >>> jonh & firstName
-- "Jonh"

{-
TASK
====
Определите тип записи, который хранит элементы лога. Имя конструктора должно
совпадать с именем типа, и запись должна содержать три поля:

- `timestamp` — время, когда произошло событие (типа `UTCTime`);
- `logLevel` — уровень события (типа `LogLevel`);
- `message` — сообщение об ошибке (типа `String`).

Определите функцию `logLevelToString`, возвращающую текстуальное представление
типа `LogLevel`, и функцию `logEntryToString`, возвращающую текстуальное
представление записи в виде:

```
<время>: <уровень>: <сообщение>
```

Для преобразование типа `UTCTime` в строку используйте функцию `timeToString`.

SOLUTION
========
-}
-- NOTE: moved to top
-- import Data.Time.Clock
-- import Data.Time.Format
-- NOTE: for GHCi 8.x we do not need this import
-- import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry = LogEntry
  { timestamp :: UTCTime,
    logLevel :: LogLevel,
    message :: String
  }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry timestamp logLevel message) =
  timeToString timestamp
    ++ ": "
    ++ logLevelToString logLevel
    ++ ": "
    ++ message

--------------------------------------------------------------------------------

-- NOTE: custom ordered fields passing
xavier :: Person
xavier =
  Person
    { age = 40,
      firstName = "Phideaux",
      lastName = "Xavier"
    }

-- >>> xavier
-- Person {firstName = "Phideaux", lastName = "Xavier", age = 40}

unknownBill =
  Person
    { firstName = "Bill"
    }

-- >>> unknownBill
-- Missing field in record construction lastName

-- >>> unknownBill & firstName
-- "Bill"

unknownBill' =
  Person
    { firstName = "Bill"
    }

-- >>> unknownBill == unknownBill'
-- Missing field in record construction lastName

updateAge :: Int -> Person -> Person
updateAge newAge person =
  person
    { age = newAge
    }

-- >>> xavier & age
-- 40

-- NOTE: returns a new record
-- >>> updateAge 42 xavier
-- Person {firstName = "Phideaux", lastName = "Xavier", age = 42}

-- >>> xavier & age
-- 40

{-
TASK
====
Определите функцию `updateLastName person1 person2`, которая меняет фамилию
`person2` на фамилию `person1`.

SOLUTION
========
-}
updateLastName :: Person -> Person -> Person
updateLastName (Person _ lastName1 _) person2 =
  person2
    { lastName = lastName1
    }

--------------------------------------------------------------------------------

name :: Person -> String
name person = firstName person ++ " " ++ lastName person

name' :: Person -> String
name' (Person fn ln _) = fn ++ " " ++ ln

-- NOTE: usefull if our record has many fields
name'' :: Person -> String
name'' Person {lastName = ln, firstName = fn} = fn ++ " " ++ ln

{-
TASK
====
Допустим мы объявили тип

```
data Shape = Circle Double | Rectangle Double Double
```

Что произойдет при объявлении такой функции:

```
isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False
```

SOLUTION
========
( ) Она не компилируется, так как объявление типа `Shape` не использует синтаксис
    записей
( ) Она не компилируется из-за синтаксической ошибки
( ) Она компилируется и всегда возвращает `True`
( ) Она компилируется и всегда возвращает `False`
(x) Она компилируется и возвращает `True`, если на вход передается `Rectangle`,
    иначе она возвращает `False`
-}

--------------------------------------------------------------------------------

{-
TASK
====
Определить функцию `abbrFirstName`, которая сокращает имя до первой буквы
с точкой, то есть, если имя было `"Ivan"`, то после применения этой функции оно
превратится в "I.". Однако, если имя было короче двух символов,
то оно не меняется.

SOLUTION
========
-}
abbrFirstName :: Person -> Person
abbrFirstName person@Person {firstName = fn}
  | length fn > 2 = person {firstName = head fn : "."}
  | otherwise = person

--------------------------------------------------------------------------------
