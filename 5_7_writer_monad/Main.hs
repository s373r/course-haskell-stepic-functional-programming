{-
Writer monad
============
-}

import Control.Monad
import Control.Monad.Writer
import Data.Monoid

{-
NOTE: simplified implementation

newtype Writer w a = Writer {runWriter :: (a, w)}

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
     in Writer (y, u `mappend` v)
-}

-- >>> runWriter (return 3 :: Writer String Int)
-- >>> runWriter (return 3 :: Writer (Sum Int) Int)
-- >>> runWriter (return 3 :: Writer (Product Int) Int)
-- >>> execWriter (return 3 :: Writer String Int)
-- (3,"")
-- (3,Sum {getSum = 0})
-- (3,Product {getProduct = 1})
-- ""

{-
TASK
====
Функция `execWriter` запускает вычисление, содержащееся в монаде `Writer`,
и возвращает получившийся лог, игнорируя сам результат вычисления. Реализуйте
функцию `evalWriter`, которая, наоборот, игнорирует накопленный лог и возвращает
только результат вычисления.

SOLUTION
========
-}
evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

-- OR
-- evalWriter m = fst (runWriter m)

--------------------------------------------------------------------------------

{-
TASK
====
Выберите все верные утверждения про монаду `Writer`.

SOLUTION
========
[x] В качестве типа результата вычисления можно использовать произвольную группу
[x] В качестве типа результата вычисления можно использовать произвольный тип
[x] В качестве типа лога можно использовать произвольную группу
[x] В качестве типа результата вычисления можно использовать произвольный моноид
[x] В качестве типа лога можно использовать произвольный моноид
[x] Тип результата вычисления и тип лога могут как совпадать, так и не совпадать
-}

--------------------------------------------------------------------------------

{-
tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w)
-}

calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
  let res = arg1 `op` arg2
  tell "ok "
  if abs res < 128
    then return res
    else do
      tell "overflow"
      return res

-- NOTE: without "ok"
-- >>> execWriter $ calc (+) 33 44
-- >>> runWriter $ calc (+) 33 44
-- >>> runWriter $ calc (+) 99 44
-- ""
-- (77,"")
-- (143,"overflow")

-- NOTE: with "ok"
-- >>> execWriter $ calc (+) 33 44
-- >>> execWriter $ calc (+) 99 44
-- "ok "
-- "ok overflow"

{-
TASK
====
Давайте разработаем программное обеспечение для кассовых аппаратов одного
исландского магазина. Заказчик собирается описывать товары, купленные
покупателем, с помощью типа `Shopping` следующим образом:

```
type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328
```

Последовательность приобретенных товаров записывается с помощью do-нотации.
Для этого используется функция `purchase`, которую вам предстоит реализовать.
Эта функция принимает наименование товара, а также его стоимость
в исландских кронах (исландскую крону не принято делить на меньшие единицы,
потому используется целочисленный тип `Integer`). Кроме того, вы должны
реализовать функцию `total`:

GHCi> total shopping1
19708

SOLUTION
========
-}
type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

purchase :: String -> Integer -> Shopping
purchase _ = tell . Sum

total :: Shopping -> Integer
total = getSum . execWriter

-- >>> total shopping1 == 19708
-- True

--------------------------------------------------------------------------------

{-
TASK
====
Измените определение типа `Shopping` и доработайте функцию purchase
из предыдущего задания таким образом, чтобы можно было реализовать
функцию `items`, возвращающую список купленных товаров (в том же порядке,
в котором они были перечислены при покупке):

SOLUTION
========
-}

type Shopping' = Writer (Sum Integer, [String]) ()

shopping2 :: Shopping'
shopping2 = do
  purchase' "Jeans" 19200
  purchase' "Water" 180
  purchase' "Lettuce" 328

purchase' :: String -> Integer -> Shopping'
purchase' item cost = tell (Sum cost, [item])

total' :: Shopping' -> Integer
total' = getSum . fst . execWriter

items' :: Shopping' -> [String]
items' = snd . execWriter

-- >>> total' shopping1 == 19708
-- >>> items' shopping2 == ["Jeans","Water","Lettuce"]
-- True
-- True

--------------------------------------------------------------------------------
