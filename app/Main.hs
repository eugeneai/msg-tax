{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

-- import Prelude.Compat
import Prelude.Compat
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)


data Message = Message {
    text::String
  , subjects::String
  } deriving (Show, Generic)


instance FromJSON Message
instance ToJSON Message




-- import MyLib (someFunc)

main :: IO ()
main = do
  d <- getContents
  putStr d
