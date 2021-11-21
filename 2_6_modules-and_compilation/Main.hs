{-
Modules and compilation
=============
-}

-- import Data.Char -- import all public functions

-- import Data.Char hiding (toLower) -- import all public functions expect
--                                      `toLower`

-- >>> toLower 'C'
-- 'c'

import Data.Char (toLower, toUpper)
-- >>> toUpper 'a'
-- 'A'

import Data.List
import qualified Data.Set
--     ↑↑↑↑↑↑↑↑↑
--     ╰┴┴┴┴┴┴┴┤
--             ╰──╮
-- NOTE: output without `qualified`
-- > :t union
-- Ambiguous occurrence `union'
-- It could refer to
--     either `Data.Set.union',
--             imported from `Data.Set'
--             (and originally defined in `Data.Set.Internal')
--         or `Data.List.union',
--             imported from `Data.List'
--             (and originally defined in `base-4.14.3.0:Data.OldList')

-- import qualified Data.Set as Set
-- > :t
-- Set.union :: Ord a => ...

{-
TASK
====
Что произойдет при попытке загрузить данный модуль в GHCi?

> module Test where
> import Data.List hiding (union)
> import Data.Set
>
> myUnion [] ys = ys
> myUnion xs ys = union xs ys

SOLUTION
========
( ) Все пройдет нормально
( ) Произойдет ошибка из-за неопределенности при выборе функции
(x) Произойдет ошибка из-за несовпадения типа аргумента и ожидаемого типа
    функции
-}

import Test

-- f1 = const42 True
--      ↑↑↑↑↑↑↑
--      we cannot use a private function from `Test` module
f2 = sumIt 3 4

-- > :l Main
-- [1 of 2] Compiling Test             ( Test.hs, interpreted )
-- [2 of 2] Compiling Main             ( Main.hs, interpreted )
-- Ok, two modules loaded.

-- > f2
-- 7

{-
TASK
====
Пусть модуль `Foo` содержит следующий код:

> module Foo (a, b) where
>
> a = undefined
> b = undefined
> c = undefined

а модуль `Bar` такой:

> module Bar (a, d) where
>
> import Foo (a, b)
>
> d = undefined

Отметьте функции, доступные для использования после загрузки в модуле `Baz`
со следующим кодом:

> module Baz where
>
> import Bar

SOLUTION
========
[ ] c
[x] d
[ ] b
[x] a
-}

{-
Compilation stages
------------------
1) Syntax parse
2) Type checking
3) Desugaring into "Core" language (lower level)
4) Optimization
5) Code generation: "Core" code --> STG machine code
6) STG machine code --> C-- ()
7) C-- --> ASM (or LLVM, depends on selected compile backend)
-}
