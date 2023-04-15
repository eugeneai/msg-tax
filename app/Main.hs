{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Prelude.Compat
    ( print,
      Show,
      Applicative((<*>)),
      String,
      Maybe,
      IO,
      (<$>),
      ($),
      putStrLn,
      FilePath )
import qualified Data.Text as T
import qualified Control.Applicative as CA
import qualified Data.Text.IO as TIO
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import System.IO
import System.IO ( print, IO, putStrLn, FilePath )
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Text.Show.Unicode as US

type MText = T.Text -- MText T.Text deriving (Show)

data Subject = Subject {
  name:: MText,
  tax:: MText
  } deriving (Show, Generic)

data MessageMorphTag = Tag String | Tags [String]
  deriving (Show, Generic)

data MessageMorph = Morph {
    norm:: MText
  , tag:: (Maybe Value)
--  , tag:: (Maybe MessageMorphTag)
  } deriving (Show, Generic)

data MessageText = Text {
    w:: MText
  , ucto:: MText
  , morph:: (Maybe MessageMorph)
  } deriving (Show, Generic)

data Message = Message {
    text:: [MessageText]
  , subjects:: (Maybe [Subject])
  } deriving (Show, Generic)


-- instance FromJSON MText where
--   parseJSON (String t) = CA.pure $ tx
--     where
--       tx = t
-- --      tx = MText (t)
--   parseJSON _ = CA.empty

-- instance ToJSON MText where
-- --  toJSON (MText t) = String t
--   toJSON t = String t

instance FromJSON Subject
instance FromJSON Message
instance FromJSON MessageText

instance FromJSON MessageMorph
instance FromJSON MessageMorphTag

instance ToJSON Subject
instance ToJSON Message
instance ToJSON MessageText

instance ToJSON MessageMorph
instance ToJSON MessageMorphTag




-- import MyLib (someFunc)

-- main :: IO ()
-- main = do
--   d <- getContents
--   putStr d


translateFile:: FilePath -> IO ()
translateFile filePath = do
  a <- BL.readFile filePath
  let obj = decode a::Maybe Message
  print obj

main :: IO ()
main = do
  putStrLn "Running main"
  js <- BL.getContents
  -- BL.putStrLn js
  let obj = decode js::Maybe Message
  US.uprint obj
