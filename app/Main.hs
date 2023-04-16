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
  US.uprint obj
  putStrLn "\n\n"
  US.uprint . NL.toText $ obj
  putStrLn "\n\n"
  US.uprint . NL.toNorm $ obj
  putStrLn "\n-------------\n"
  let tran = NL.toJoin obj
  let joinRule = NL.join NL.AdjNoun
  case tran of
    Nothing -> print "No parse"
    Just appl -> do
      let appl1 = NL.joinPass NL.AdjNoun appl
      US.uprint appl1
--       let appl2 = NL.joinPass NL.SubjVerb appl
