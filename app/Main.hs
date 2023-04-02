module Main where

import Prelude

-- import MyLib (someFunc)

main :: IO ()
main = do
  d <- getContents
  putStr d
