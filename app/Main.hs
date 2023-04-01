module Main where

import Prelude

-- import MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!" >> putStrLn "!!!"
  -- MyLib.someFunc
