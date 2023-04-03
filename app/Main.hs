{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Main (translate, main) where

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
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import System.IO
import System.IO ( print, IO, putStrLn, FilePath )
import Data.Aeson
import Data.Aeson.Types
import Data.Text
data Subject = Subject {
  name:: Text,
  tax:: Text
  } deriving (Show, Generic)

data MessageMorphTag = Tag String | Tags [String]
  deriving (Show, Generic)

data MessageMorph = Morph {
    norm:: Text
  , tag:: (Maybe Value)
--  , tag:: (Maybe MessageMorphTag)
  } deriving (Show, Generic)

data MessageText = Text {
    w:: Text
  , ucto:: Text
  , morph:: (Maybe MessageMorph)
  } deriving (Show, Generic)

data Message = Message {
    text:: [MessageText]
  , subjects:: (Maybe [Subject])
  } deriving (Show, Generic)


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

translate :: BL.ByteString -> Maybe Message
translate = decode

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
  print obj
