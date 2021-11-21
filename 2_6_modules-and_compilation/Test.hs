module Test (sumIt) where

--           ↑↑↑↑↑
--           export only `sumIt` function
--           `const42` is a private function now

sumIt x y = x + y

const42 = const 42
