module Main (main) where

import qualified MyLib as NL
import qualified Text.Show.Unicode as US
import qualified Data.ByteString.Lazy.Char8 as BL
import Prelude.Compat (id)

repeatPass p appl =
  let a2 = p appl in
    if a2 /= appl then repeatPass p a2
    else a2

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
      let appl2 = repeatPass NL.joinPass appl
      US.uprint appl2
