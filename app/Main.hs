module Main (main) where

import qualified MyLib as NL
import qualified Text.Show.Unicode as US
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  putStrLn "Running main"
  js <- BL.getContents
  -- BL.putStrLn js
  let obj = NL.translateContent js::Maybe NL.Message
  US.uprint obj
  US.uprint . NL.toText $ obj
  putStrLn "\n\n"
  US.uprint . NL.toNorm $ obj
