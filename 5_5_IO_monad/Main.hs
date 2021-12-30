{-
IO monad
========
-}
import Control.Monad
import Control.Monad.Writer (guard)
import Data.List as L (isInfixOf)

main :: IO ()
main = do
  putStrLn "What is your name"
  name <- getLine
  putStrLn $ "Nice to meet yoy, " ++ name ++ "!"

-- >>> :t main
-- >>> :t putStrLn
-- main :: IO ()
-- putStrLn :: String -> IO ()

{-
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
*Main> main
What is your name
Dima
Nice to meet yoy, Dima!
-}

{-
TASK
====
На этом шаге вы будете работать с монадой `IO`, а значит, ваша программа будет
взаимодействовать с операционной системой. Чтобы тестирующая система смогла
оценить вашу программу, пожалуйста, используйте только функции, осуществляющие
ввод/вывод на терминал: `getChar`, `putChar`, `putStr`, `putStrLn`, `getLine`.
Все эти функции уже будут находиться в области видимости, так что вам не следует
их импортировать. По той же причине, главная функция вашей программы будет
называться не `main`, а `main'` (со штрихом).

Напишите программу, которая будет спрашивать имя пользователя, а затем
приветствовать его по имени. Причем, если пользователь не ввёл имя, программа
должна спросить его повторно, и продолжать спрашивать, до тех пор, пока
пользователь не представится.

Итак, первым делом, программа спрашивает имя:

```
What is your name?
Name:
```

Пользователь вводит имя и программа приветствует его:

```
What is your name?
Name: Valera
Hi, Valera!
```

Если же пользователь не ввёл имя, необходимо отобразить точно такое же
приглашение ещё раз:

```
What is your name?
Name:
What is your name?
Name:
What is your name?
Name: Valera
Hi, Valera!
```

Пожалуйста, строго соблюдайте приведенный в примере формат вывода.
Особое внимание уделите пробелам и переводам строк! Не забудьте про пробел после
`Name:`, а также про перевод строки в самом конце (ожидается, что вы будете
использовать `putStrLn` для вывода приветствия пользователя).

SOLUTION
========
-}
main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "

  name <- getLine

  if name /= ""
    then putStrLn $ "Hi, " ++ name ++ "!"
    else main'

--------------------------------------------------------------------------------

{-
getCharFromConsole :: Char

getCharFromConsole :: RealWorld -> (RealWorld, Char)
-}

{-
newtype IO a = IO (RealWorld -> (RealWorld, a))
-}

-- >>> :k IO
-- IO :: * -> *

--------------------------------------------------------------------------------

{-
type IO a = (RealWorld -> (RealWorld, a))

return :: a -> RealWorld -> (RealWorld, a)

(>>=) ::
  (RealWorld -> (RealWorld, a))
  -> (a -> RealWorld -> (RealWorld, b))
  -> RealWorld -> (RealWorld, b)

instance Monad IO where
  return a = \w -> (w, a)

  (>>=) m k =
    \w -> case m w of
      (w', a) -> k a w'
-}

--------------------------------------------------------------------------------

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n'
    then return []
    else do
      cs <- getLine'
      return (c : cs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

--------------------------------------------------------------------------------

{-
sequence_ :: Monad M => [m a] -> m ()
sequence_ = foldr (>>) (return ())
-}

-- >>> sequence_ [Just 1, Just 2]
-- >>> sequence_ [Just 1, Nothing]
-- Just ()
-- Nothing

-- >>> sequence_ [[1, 2], [3, 4, 5, 6]]
-- [(),(),(),(),(),(),(),()]

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar

{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f
-}

-- >>> mapM_ (\x -> [x, x]) "ABC"
-- >>> map (\x -> [x, x]) "ABC"
-- [(),(),(),(),(),(),(),()]
-- ["AA","BB","CC"]

putStr''' :: String -> IO ()
putStr''' = mapM_ putChar

{-
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
  where
    k :: Monad m => m a -> m [a] -> m [a]
    k m m' = do
      x <- m
      xs <- m'
      return (x: xs)

mapM :: Momad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
-}

-- >>> sequence_ [Just 1, Just 2]
-- >>> sequence [Just 1, Just 2]
-- >>> sequence [Just 1, Nothing]
-- Just ()
-- Just [1,2]
-- Nothing

{-
TASK
====
На этом шаге вы будете работать с монадой `IO`, а значит, ваша программа будет
взаимодействовать с операционной системой. Чтобы тестирующая система смогла
оценить вашу программу, пожалуйста, используйте только функции, работающие с
файлами и директориями: `getDirectoryContents`, `removeFile`. Все эти функции
уже будут находиться в области видимости, так что вам не следует их
импортировать. По той же причине, главная функция вашей программы будет
называться не `main`, а `main'` (со штрихом).

В этом задании ваша программа должна попросить пользователя ввести любую строку,
а затем удалить все файлы в текущей директории, в именах которых содержится эта
строка, выдавая при этом соответствующие сообщения.

```
Substring:
```

Пользователь вводит любую строку:

```
Substring: hell
```

Затем программа удаляет из текущей директории файлы с введенной подстрокой
в названии. К примеру, если в текущей директории находились файлы `thesis.txt`,
`kitten.jpg`, `hello.world`, `linux_in_nutshell.pdf`, то вывод будет таким:

```
Substring: hell
Removing file: hello.world
Removing file: linux_in_nutshell.pdf
```

Если же пользователь ничего не ввёл (просто нажал Enter), следует ничего
не удалять и сообщить об этом:

```
Substring:
Canceled
```

Для получения списка файлов в текущей директории используйте функцию
`getDirectoryContents`, передавая ей в качестве аргумента строку, состоящую
из одной точки  (`"."`), что означает «текущая директория». Для удаления файлов
используйте функцию `removeFile` (считайте, что в текущей директории
нет поддиректорий — только простые файлы). В выводимых сообщениях удаленные
файлы должны быть перечислены в том же порядке, в котором их возвращает
функция `getDirectoryContents`.

Пожалуйста, строго соблюдайте приведенный в примере формат вывода. Особое
внимание уделите пробелам и переводам строк! Не забудьте про пробел после
`Substring:`, а также про перевод строки в конце (ожидается, что вы будете
использовать `putStrLn` для вывода сообщений об удалении).

SOLUTION
========
-}

-- NOTE: just for code competition
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = undefined

removeFile :: FilePath -> IO ()
removeFile = undefined

-- NOTE: imported above
--       used `as L` since `import Data.List` has already imported in tests
--       but with specific functions
-- import Data.List as L (isInfixOf)

-- NOTE: main'' is used since main' has already exist
main'' :: IO ()
main'' = do
  putStr "Substring: "
  mask <- getLine

  if mask /= ""
    then do
      files <- getDirectoryContents "."

      let filesForRemoval = [file | file <- files, mask `isInfixOf` file]

      forM_ filesForRemoval $ \file -> do
        putStrLn ("Removing file: " ++ file)
        removeFile file
    else putStrLn "Canceled"

--------------------------------------------------------------------------------
