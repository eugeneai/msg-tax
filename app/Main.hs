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

-- repeatPass p appl =
--   let a2 = p appl in
--     if a2 /= appl then repeatPass p a2
--     else a2


inputJSON :: Handle -> IO BL.ByteString
inputJSON hout = do
  bs_len <- BL.hGet hout 10
  let mjs_len = BL.readInt bs_len
  case mjs_len :: Maybe (Int, BL.ByteString) of
    Nothing -> pure $ BL.pack ""
    Just (js_len, _) -> do
      _ <- BL.hGet hout 1 -- Space between size and JSON
      js <- BL.hGet hout js_len
      return js


data ProcessData = Proc {
  hin :: Handle,
  hout :: Handle,
  herr :: Handle,
  ph :: ProcessHandle }

subProcess :: IO (ProcessData)
subProcess = do
  (Just hin, Just hout, Just herr, pid) <- createProcess
    -- (proc "/home/eugeneai/.pyenv/shims/python" ["app/tokenizer.py", "-p"])
    (proc "/home/eugeneai/.pyenv/shims/python" ["app/tokenizer.py", "-p"])
    {cwd = Just "/home/eugeneai/projects/code/haskell/msg-tax",
     std_out = CreatePipe,
     std_in = CreatePipe,
     std_err = CreatePipe}
  hSetEncoding hin utf8
  hSetEncoding hout utf8
  hSetEncoding herr utf8
  return $ Proc hin hout herr pid


processString :: ProcessData -> [[NL.Gram]] -> String -> IO ([[NL.Gram]])
processString (Proc hin hout herr ph) prev str = do
  hPutStrLn hin $ "WORD " ++ str
  hFlush hin
  js <- inputJSON hout
  -- US.uprint js
  let obj = NL.translateLexs js
  -- US.uprint obj
  -- putStrLn "\n-------------\n"
  case obj of
    Nothing -> return (prev)
    Just o -> do
      let tran = NL.recognize prev o :: [[NL.Gram]]
      -- US.uprint tran
      return tran

processTerminate :: ProcessData -> IO ()
processTerminate (Proc hin hout herr ph) = do
  BL.hPutStrLn hin . BL.pack $ "QUIT"
  hFlush hin
  waitForProcess ph
  hClose hin


stdinProc :: IO ()
stdinProc = do
  putStrLn "Processing stdin"
  js <- BL.getContents
  -- BL.putStrLn js
  let obj = NL.translateContent js::Maybe NL.Message
  -- US.uprint obj
  -- putStrLn "\n-------------\n"
  case obj of
    Nothing -> return ()
    Just o -> do
      let tran = NL.recognize [[]] (NL.text o) :: [[NL.Gram]]
      printGrams tran
  -- US.uprint "OK"
  -- US.uprint obj
--  putStrLn "\n\n"
--  US.uprint . NL.toText $ obj
  -- putStrLn "\n\n"
  -- US.uprint . NL.toNorm $ obj
  -- putStrLn "\n-------------\n"
  -- let tran = NL.toJoin obj
  -- case tran of
  --   Nothing -> print "No parse"
  --   Just appl -> do
  --     let appl2 = repeatPass NL.joinPass appl
  --     US.uprint appl2

printGrams :: [[NL.Gram]] -> IO ()
printGrams [] = do
  return ()
printGrams (grs:rest) = do
  US.uprint . reverse $ grs
  putStrLn ""
  printGrams rest

main :: IO ()
main = do
  args <- getArgs
  if elem "-t" args then do stdinProc
    else if elem "-s" args then do
    let str = unwords . tail . L.takeWhile (/="-t") $ args
    pyproc <- subProcess
    j <- processString pyproc [[]] $ str
    printGrams j
    processTerminate pyproc
    else putStrLn "Wrong arguments"
