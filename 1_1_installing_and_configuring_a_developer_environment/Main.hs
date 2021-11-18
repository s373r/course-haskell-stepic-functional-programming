{-
Information
===========

Tools
-----
 GHC - compiler
GHCi - interpreter (REPL)

Source
------
.hs - text files

Indentation
-----------
a tab (\t) character == 8 spaces

Language version
----------------
Haskell 2010 is the latest version

Haskell
-------
is a general-purpose, statically typed, purely functional programming language
with type inference and lazy evaluation

It is named after logician Haskell Curry

Documentation
-------------
https://www.haskell.org
https://www.haskell.org/hoogle/
-}

{-
GHCi
====

Prelude
-------
is imported by default into all Haskell modules unless either there is
an explicit import statement for it, or the NoImplicitPrelude extension
is enabled

Set prompt
----------
:set prompt "GHCi> "

-}

-- >>> 33 + 3 * 3
-- 42

-- >>> pi
-- 3.141592653589793

-- >>> "ABC" ++ "DE"
-- "ABCDE"

{-
TASK
====
Какое приглашение на самом деле выдает командная строка интерпретатора
(в предыдущем примере интерпретатор выдал приглашение GHCi> )?

SOLUTION
========
*Main>
-}

{-
Import in GHCi
--------------
> :load Test -- or :l Test
> sayHello
Hello from module Test!

Reload changed modules
----------------------
:reload [Module]
:r      [Module]

> :r Test
> sayHello
Hello World from module Test!
-}
