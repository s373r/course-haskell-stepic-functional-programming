{-
Reader monad
============
-}
import Control.Monad (ap, liftM)

{-
instance Functor ((->) e) where -- `e` means environment
  fmap g h = g . h

fmap :: (a -> b) -> f a -> f b
     :: (a -> b) -> (e -> a) -> (e -> b)
-}

-- >>> :t fmap (^2) length
-- fmap (^2) length :: Foldable t => t a -> Int

-- >>> fmap (^2) length [1, 2, 3]
-- 9

{-
instance Monad ((->) e) where
  return :: a -> e -> a
  return x = \_ -> x

  (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
  m >>= k = \e -> k (m e) e
-}

safeHead :: [a] -> Maybe a
safeHead = do
  b <- null
  if b
    then return Nothing
    else do
      h <- head
      return $ Just h

-- >>> safeHead []
-- >>> safeHead [1, 2]
-- Nothing
-- Just 1

safeHead' :: [a] -> Maybe a
safeHead' = do
  e <- id
  if null e
    then return Nothing
    else return $ Just (head e)

{-
TASK
====
Не используя интерпретатор, вычислите значение следующего выражения:

```
return 2 >>= (+) >>= (*) $ 4
```

SOLUTION
========
24
-}

--------------------------------------------------------------------------------

{-
TASK
====
При работе с монадой `Reader`, каков смысл оператора `(>>)`?

SOLUTION
========
[ ] Этот оператор позволяет передать одно и то же значение (окружение)
    в качестве аргумента нескольким функциям в цепочке композиций
[ ] Этот оператор позволяет изменить окружение
[ ] Этот оператор позволяет вычислить произвольную функцию от окружения
[x] В сочетании с монадой Reader этот оператор бесполезен
-}

--------------------------------------------------------------------------------

newtype Reader r a = Reader {runReader :: r -> a}

-- NOTE: from GHCi 7.10, we have to implement Applicative for our type
--       based on https://stepik.org/lesson/8437/step/8?discussion=117854&reply=117875&unit=1572
instance Functor (Reader r) where
  fmap = liftM

instance Applicative (Reader r) where
  pure = return
  (<*>) = ap

--

instance Monad (Reader r) where
  return x = Reader $ \e -> x
  m >>= k = Reader $ \e ->
    let v = runReader m e
     in runReader (k v) e

ask :: Reader r r
ask = Reader id

-- >>> runReader ask 42
-- 42

type User = String

type Password = String

type UsersTable = [(User, Password)]

pwds :: UsersTable
pwds =
  [ ("Bill", "123"),
    ("Ann", "qwerty"),
    ("John", "2sRq8p")
  ]

firstUser :: Reader UsersTable User
firstUser = do
  e <- ask
  return $ fst (head e)

-- >>> runReader firstUser pwds
-- "Bill"

--------------------------------------------------------------------------------

asks :: (r -> a) -> Reader r a
asks = Reader

-- firstUserPwd :: Reader UsersTable Password
-- firstUserPwd = do
--   pwd <- asks (snd . head)
--   return pwd

firstUserPwd :: Reader UsersTable Password
firstUserPwd = asks (snd . head)

-- >>> runReader firstUserPwd pwds
-- "123"

usersCount :: Reader UsersTable Int
usersCount = asks length

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

localTest :: Reader UsersTable (Int, Int)
localTest = do
  count1 <- usersCount
  count2 <- local (("Mike", "1") :) usersCount
  return (count1, count2)

-- >>> runReader localTest pwds
-- >>> runReader localTest []
-- (3,4)
-- (0,1)

reader :: (r -> a) -> Reader r a
reader f = do
  r <- ask
  return (f r)

{-
TASK
====
В последнем видео мы познакомились с функцией `local`, позволяющей произвести
некоторое вычисление во временно измененном окружении. При этом значение,
задающее новое окружение, имело тот же тип, что и исходное.

Если попытаться обобщить эту функцию таким образом, чтобы новое окружение
потенциально имело другой тип, какая сигнатура будет у обобщенной
функции `local'`?

SOLUTION
========
( ) local' :: (r -> r') -> Reader r a -> Reader r' a
( ) Такая обобщенная функция не может существовать
( ) local' :: (r' -> r) -> Reader r' a -> Reader r a
(x) local' :: (r -> r') -> Reader r' a -> Reader r ()
( ) local' :: (r -> e) -> Reader e a -> Reader r a
-}

--------------------------------------------------------------------------------

{-
TASK
====
Реализуйте функцию `local'` из прошлого задания.

Считайте, что монада `Reader` определена так, как на видео:

```
data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r
```

SOLUTION
========
-}
local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

--------------------------------------------------------------------------------

{-
TASK
====
Вспомним пример с базой пользователей и паролей:

```
type User = String
type Password = String
type UsersTable = [(User, Password)]
```

Реализуйте функцию, принимающую в качестве окружения `UsersTable`
и возвращающую список пользователей, использующих пароль `"123456"`
(в том же порядке, в котором они перечислены в базе).

```
GHCi> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
["user","root"]
```

SOLUTION
========
-}

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = reader f
  where
    f :: UsersTable -> [User]
    f table = [user | (user, password) <- table, password == "123456"]

-- >>> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")] == ["user","root"]
-- True

--------------------------------------------------------------------------------
