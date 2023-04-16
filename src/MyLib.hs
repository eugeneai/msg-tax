{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib (
  Subject,
  MorphTag,
  Morph,
  Lex,
  Message,
  translateContent,
  toText,
  toNorm,
  convertMsg,
--  translateFile,
  version
             ) where

import Prelude.Compat
    (
      Show
      -- Applicative((<*>)),
    , String
    , Maybe
    , Maybe (Nothing)
    , Maybe (Just)
      -- IO,
      --- (<$>),
    , ($)
    , (.)
    , map
    , concatMap
      -- FilePath
    )
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Control.Applicative as CA
-- import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
-- import System.IO
-- import System.IO ( print, IO, putStrLn, FilePath )
import Data.Aeson
-- import Data.Aeson.Types
-- import Data.Text
-- import qualified Text.Show.Unicode as US

data Subject = Subject {
  name:: T.Text,
  tax:: T.Text
  } deriving (Show, Generic)

data MorphTag = Tag String | Tags [String]
  deriving (Show, Generic)

newtype TagSet = TagSet (Set.Set T.Text) deriving Show

data Morph = Morph {
    norm:: T.Text
  , tag:: TagSet
--  , tag:: (Maybe MessageMorphTag)
  } deriving (Show, Generic)

data Lex = Lex {
    w:: T.Text
  , ucto:: T.Text
  , morph:: (Maybe Morph)
  } deriving (Show, Generic)

data Message = Message {
    text:: [Lex]
  , subjects:: (Maybe [Subject])
  } deriving (Show, Generic)


instance FromJSON Subject
instance FromJSON Message
instance FromJSON Lex

instance FromJSON Morph
instance FromJSON MorphTag
instance FromJSON TagSet where

  parseJSON (Array v) = CA.pure . TagSet $ myUnpack . V.toList $ v
    where

      myUnpack values = Set.fromList $ concatMap val values
      val :: Value -> [T.Text]
      val (String s) = [s]
      val (Array a) = concatMap val . V.toList $ a
      val _ = ["IGN"]
  parseJSON _ = CA.empty

instance ToJSON Subject
instance ToJSON Message
instance ToJSON Lex
instance ToJSON Morph

instance ToJSON TagSet where

  toJSON (TagSet s) = Array $ V.empty

instance ToJSON MorphTag


-- translateFile:: FilePath -> IO (Maybe Message)
-- translateFile filePath = do
--   a <- BL.readFile filePath
--   let obj = decode a::Maybe Message
--   IO (obj)

translateContent :: BL.ByteString -> Maybe Message
translateContent content = do
  let obj = decode content :: Maybe Message
  obj

defNoParse :: T.Text
defNoParse = (T.pack "Error: no parse")

toText :: Maybe Message -> T.Text
toText = convertMsg defNoParse w

toNorm :: Maybe Message -> T.Text
toNorm = convertMsg defNoParse c
  where
    c lex = case morph lex of
      Nothing -> w lex
      Just m -> norm m

-- Lex {w = "база", ucto = "word", morph = Just (Morph {norm = "база", tag = Just (Array [String "noun",String "inan",Array [String "femn",String "sing"],String "nomn"])})}

convertMsg :: T.Text -> (Lex -> T.Text) -> Maybe Message -> T.Text
convertMsg def _ Nothing = def
convertMsg _ c (Just msg) = T.intercalate (T.pack " ") . map c $ text msg

version :: String
version = "0.0.1"
