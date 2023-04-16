{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib (
    Subject
  , MorphTag
  , Morph
  , Lex
  , Message
  , translateContent
  , toText
  , toNorm
  , convertMsg
  , toJoin
  , joinPass
  , Connection (AdjNoun, SubjVerb)
  , join
  , text
  , version) where

import Prelude.Compat
    -- (
    --   Show
    --   -- Applicative((<*>)),
    -- , String
    -- , Maybe
    -- , Maybe (Nothing)
    -- , Maybe (Just)
    --   -- IO,
    --   --- (<$>),
    -- , ($)
    -- , (.)
    -- , map
    -- , concatMap
    -- , maybe
    -- , show
    -- , (++)
    -- , otherwise
    -- , Bool
    -- , Bool (False)
    -- , Bool (True)
    -- , all
    -- , any
    -- , (&&)
    -- , (||)
    --   -- FilePath
    -- )
-- import BasePrelude (intercalate)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Text.Read.Compat as TR
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

newtype TagSet = TagSet (Set.Set GRAM)

instance Show TagSet where
  show (TagSet s)
    | Set.null s = "*"
    | otherwise = show . Set.toList $ s

data Morph = Morph {
    norm:: T.Text
  , tag:: TagSet
  , score:: Float
--  , tag:: (Maybe MessageMorphTag)
  } deriving (Show, Generic)

data Lex = Lex {
    w:: T.Text
  , ucto:: T.Text
  , morph:: Maybe [Morph]
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
      myUnpack :: [Value] -> Set.Set GRAM
      myUnpack values = Set.fromList $ concatMap val values
      val :: Value -> [GRAM]
      val (String s) = [readGram s]
      val (Array a) = concatMap val . V.toList $ a
      val _ = [BAD]
  parseJSON _ = CA.empty

readGram :: T.Text -> GRAM
readGram t =
  let rm = TR.readMaybe . T.unpack . T.toUpper $ t :: Maybe GRAM
  in
    case rm of
      Nothing -> Unrec t
      Just g -> g :: GRAM

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
      Just morphs -> go morphs -- T.concatMap gonorm morphs
    go :: [Morph] -> T.Text
    go xs = T.intercalate "/" . map gonorm $ xs :: T.Text
    gonorm :: Morph -> T.Text
    gonorm = norm

convertMsg :: T.Text -> (Lex -> T.Text) -> Maybe Message -> T.Text
convertMsg def _ Nothing = def
convertMsg _ c (Just msg) = T.intercalate (T.pack " ") . map c $ text msg

version :: String
version = "0.0.1"

data Connection = AdjNoun | SubjVerb | Next deriving Show

--                                  ucto   norm   tagSet
data Join = J Connection [Join] | L T.Text [Morph]

instance Show Join where
  show (J conn xs) = "(" ++ show conn ++ " " ++ tail ++ ")"
    where
      tail = concatMap show $ xs
  show (L u ts) = "|" ++ (T.unpack u) ++
    " " ++ show ts ++ "\n"

toJoin :: Maybe Message -> Maybe Join
toJoin Nothing = Nothing
toJoin (Just msg) = Just . J Next . map lex . text $ msg
  where
    lex l = L (ucto l) (maybe [] id $ morph l)
    -- mynorm ::Lex -> T.Text
    -- mynorm l = maybe (w l) norm (morph l)
    -- mytagset :: Lex -> TagSet
    -- mytagset l = maybe (TagSet Set.empty) tag (morph l)


class Rule r where
  join :: r -> Join -> Join -> Maybe Join

joinPass :: (Rule r) => r -> Join -> Join
joinPass r (J Next l) = J Next $ applyRule r l
joinPass r rest = rest

applyRule :: (Rule r) => r -> [Join] -> [Join]
applyRule _ [] = []
applyRule _ [e] = [e]
applyRule r (a:b:l) = rc
  where
    appl = join r a b
    rc = case appl of
      Nothing -> a:(applyRule r (b:l))
      Just j -> applyRule r (j:l)

data GRAM = NOUN
  | ADJF
  | ADJS
  | COMP
  | VERB
  | INFN
  | PRTF
  | PRTS
  | GRND
  | NUMR
  | ADVB
  | NPRO
  | PRED
  | PREP
  | CONJ
  | PRCL
  | INTJ
  | NOMN
  | GENT
  | DATV
  | ACCS
  | ABLT
  | LOCT
  | VOCT
  | GEN2
  | ACC2
  | LOC2
  | SING
  | PLUR
  | MASC
  | FEMN
  | NEUT
  | LATN
  | PNCT
  | NUMB
  | INTG
  | REAL
  | ROMN
  | UNKN
  | PER1
  | PER2
  | PER3
  | BAD
  | Unrec T.Text -- For all unknown
  deriving (Show, Read, Ord, Eq)

-- Simple Grammar rules

instance Rule Connection where
  join AdjNoun adj noun
    | isAdj adj && isNoun noun && adjNounConsist adj noun = Just (J AdjNoun [adj, noun])
  -- join AdjNoun a b = Nothing
  join SubjVerb subj verb
    | isSubj subj && isVerb verb && subjVerbConsist subj verb =
      Just (J SubjVerb [subj, verb])
  join _ _ _ = Nothing

lexTest :: [GRAM] -> Join -> Bool
lexTest [] _ = True
lexTest sub (L _ morphs) = False -- rc
  -- where
  --   tt = map T.pack sub
  --   rc = all (\e -> Set.member e ts) tt
lexTest sub (J _ []) = False
lexTest sub (J _ (a:_)) = lexTest sub a


isVerb :: Join -> Bool
isVerb = lexTest [VERB]

isSubj :: Join -> Bool
isSubj v = isNounNomn v || isPronoun v

isNounNomn :: Join -> Bool
isNounNomn n = isNoun n && isNomn n

isNomn :: Join -> Bool
isNomn n = lexTest [NOMN] n

isNoun :: Join -> Bool
isNoun a = lexTest [NOUN] a


isAdj:: Join -> Bool
isAdj a = lexTest [ADJF] a

adjNounConsist :: Join -> Join -> Bool
adjNounConsist adj noun = sameDeclination adj noun


nounCases :: Set.Set GRAM
nounCases = Set.fromList
            [NOMN, GENT, DATV,
              ACCS, ABLT, LOCT,
              VOCT, GEN2, ACC2,
              LOC2]

mult :: Set.Set GRAM
mult = Set.fromList [SING, PLUR]

data POSAttr = MULT | CASE | PRON

getProp :: [POSAttr] -> Morph -> Set.Set GRAM
getProp [CASE] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e nounCases
getProp [MULT] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e mult
getProp [PRON] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e pronouns
getProp [] _ = Set.empty:: Set.Set GRAM
getProp (a:t) ts = getProp [a] ts `Set.union` getProp t ts

unTS :: TagSet -> Set.Set GRAM
unTS (TagSet t) = t

sameDeclination :: Join -> Join -> Bool
sameDeclination a b = False -- sa =*= sb
  -- where
  --   as = [CASE, MULT]
  --   sa = getProp as a
  --   sb = getProp as b

(=*=) :: Set.Set T.Text -> Set.Set T.Text -> Bool
a =*= b = Set.isSubsetOf a b && Set.isSubsetOf b a

pronouns :: Set.Set GRAM
pronouns = Set.fromList [PER1, PER1, PER3]

isPronoun :: Join -> Bool
isPronoun p = any f $ Set.toList pronouns
  where
    f pr = lexTest [pr] p

subjVerbConsist :: Join -> Join -> Bool
subjVerbConsist pr verb = cpr && cverb
  where
    pra = [PRON, MULT]
    cpr = True
    cverb = True
