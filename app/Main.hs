module Main (main) where

import qualified MyLib as NL
import qualified Text.Show.Unicode as US
import qualified Data.ByteString.Lazy.Char8 as BL
import Prelude.Compat (id)

main :: IO ()
main = do
  putStrLn "Running main"
  js <- BL.getContents
  -- BL.putStrLn js
  let obj = NL.translateContent js::Maybe NL.Message
--  US.uprint obj
--  putStrLn "\n\n"
  US.uprint . NL.toText $ obj
  putStrLn "\n\n"
  US.uprint . NL.toNorm $ obj
  putStrLn "\n-------------\n"
  let tran = NL.toJoin obj
  case tran of
    Nothing -> print "No parse"
    Just appl -> do
      let appl1 = NL.joinPass appl
      let appl2 = NL.joinPass appl1
      let appl3 = NL.joinPass appl2
      US.uprint appl3
      US.uprint (appl2 == appl1)
      US.uprint (appl3 == appl2)
