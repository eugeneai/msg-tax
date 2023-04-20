module Main (main) where

import qualified MyLib as NL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Show.Unicode as US
import qualified Data.ByteString.Lazy.Char8 as BL
import Prelude.Compat (id)
import System.Environment
import System.Process
import System.IO

repeatPass p appl =
  let a2 = p appl in
    if a2 /= appl then repeatPass p a2
    else a2



processText :: T.Text -> IO () --  Maybe NL.Join
processText str = do
  (Just hin, Just hout, Just herr, pid) <- createProcess
    -- (proc "/home/eugeneai/.pyenv/shims/python" ["app/tokenizer.py", "-p"])
    (proc "/home/eugeneai/.pyenv/shims/python" ["app/tokenizer.py", "-p"])
    {cwd = Just "/home/eugeneai/projects/code/haskell/msg-tax",
     std_out = CreatePipe,
     std_in = CreatePipe,
     std_err = CreatePipe}
  hSetEncoding hin utf8
  -- hSetEncoding hout utf8
  -- hSetEncoding herr utf8
  js <- BL.hGetContents hout
  -- jse <- BL.hGetContents herr
  hPutStrLn hin $ "WORD Мама мыла"
  hFlush hin
  let mjs_len = BL.readInt js :: Maybe (Int, BL.ByteString)
  let (js_len, rest) = maybe (0, js) id mjs_len :: (Int, BL.ByteString)
  US.uprint js_len
  g <- BL.hGet hout (js_len + 1)
  US.uprint g
  let obj = NL.translateLexs g
  -- US.uprint obj
  -- BL.putStrLn $ js
  case obj of
    Nothing -> do
      putStrLn "Error: cannot parse response"
    Just o -> do
      let tran = NL.lexsToJoin o
      putStrLn "Parsed"
      US.uprint tran

      case tran of
        Nothing -> do
          err <- BL.hGetContents herr
          putStrLn "Error:"
          BL.putStrLn err
        Just appl -> do
          let appl2 = repeatPass NL.joinPass appl
          US.uprint . take 10 . show $ appl2
  BL.hPutStrLn hin . BL.pack $ "QUIT"
  hFlush hin
  waitForProcess pid
  hClose hin


stdinProc :: IO ()
stdinProc = do
  putStrLn "Processing stdin"
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

main :: IO ()
main = do
  args <- getArgs
  if elem "-t" args then do stdinProc
    else if elem "-s" args then do
    let str = unwords . tail . L.takeWhile (/="-t") $ args
    processText . T.pack $ str
    else putStrLn "Wrong arguments"
