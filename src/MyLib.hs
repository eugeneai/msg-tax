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
import qualified Data.List as L
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
      Nothing -> case T.unpack . T.toUpper $ t of
        "1PER" -> PER1
        "2PER" -> PER3
        "3PER" -> PER3
        _ -> Unrec t
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

data Join = R [Join] | J Connection (Morph, Morph) (Join, Join) | L T.Text [Morph]

instance Show Join where
  show (J conn m (a, b)) = "(J " ++ show conn ++ " " ++ tail ++ " / "++ show m ++ ")\n"
    where
      tail = show a ++ " " ++ show b
  show (R ts) = "(R " ++ (concatMap show $ ts) ++ ")"
  show (L u ts) = "|" ++ (T.unpack u) ++
    " " ++ show ts ++ "\n"

toJoin :: Maybe Message -> Maybe Join
toJoin Nothing = Nothing
toJoin (Just msg) = Just . R . map lex . text $ msg
  where
    lex l = L (ucto l) (maybe [] id $ morph l)
    -- mynorm ::Lex -> T.Text
    -- mynorm l = maybe (w l) norm (morph l)
    -- mytagset :: Lex -> TagSet
    -- mytagset l = maybe (TagSet Set.empty) tag (morph l)


class Rule r where
  join :: r -> Join -> Join -> Maybe Join

joinPass :: (Rule r) => r -> Join -> Join
joinPass r (R l) = R $ applyRule r l
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

data GRAM = BAD
  | POST  | NOUN  | ADJF  | ADJS  | COMP  | VERB  | INFN  | PRTF  | PRTS
  | GRND  | NUMR  | ADVB  | NPRO  | PRED  | PREP  | CONJ  | PRCL  | INTJ  | ANIM
  | INAN  | GNDR  | MASC  | FEMN  | NEUT  | MS_F  | NMBR  | SING  | PLUR  | SGTM
  | PLTM  | FIXD  | CASE  | NOMN  | GENT  | DATV  | ACCS  | ABLT  | LOCT  | VOCT
  | GEN1  | GEN2  | ACC2  | LOC1  | LOC2  | ABBR  | NAME  | SURN  | PATR  | GEOX
  | ORGN  | TRAD  | SUBX  | SUPR  | QUAL  | APRO  | ANUM  | POSS  | V_EY  | V_OY
  | CMP2  | V_EJ  | ASPC  | PERF  | IMPF  | TRNS  | TRAN  | INTR  | IMPE  | IMPX
  | MULT  | REFL  | PERS  | PER1  | PE22  | PER3  | TENS  | PRES  | PAST  | FUTR
  | MOOD  | INDC  | IMPR  | INVL  | INCL  | EXCL  | VOIC  | ACTV  | PSSV  | INFR
  | SLNG  | ARCH  | LITR  | ERRO  | DIST  | QUES  | DMNS  | PRNT  | V_BE  | V_EN
  | V_IE  | V_BI  | FIMP  | PRDX  | COUN  | COLL  | V_SH  | AF_P  | INMX  | VPRE
  | ANPH  | INIT  | ADJX  | HYPO  | LATN  | UNKN
  | Unrec T.Text -- For all unknown
  deriving (Show, Read, Ord, Eq)

-- Simple Grammar rules

instance Rule Connection where
  join :: Connection -> Join -> Join -> Maybe Join
  join AdjNoun adj noun =
    let l = [ (a, n) | (a, n) <- compPairs adj noun,
              isAdj a && isNoun n && adjNounConsist a n ]
    in case l of
      [] -> Nothing
      ll -> Just (J AdjNoun (head ll) (adj, noun))

  -- join AdjNoun a b = Nothing
  -- join SubjVerb subj verb
  --   | isSubj subj && isVerb verb && subjVerbConsist subj verb =
  --     Just (J SubjVerb [subj, verb])
  join _ _ _ = Nothing

minimumScore :: Float
minimumScore = 0.01

compPairs :: Join -> Join -> [(Morph, Morph)]
compPairs a b = rc
  where
    rc = L.map g . L.sortBy f $ ([ (calcS aa bb, aa, bb) | aa <- getParJ a, bb <- getParJ b,
                                  calcS aa bb > minimumScore])
    g (_, b', c') = (b',c')
    f :: (Float, Morph, Morph) -> (Float, Morph, Morph) -> Ordering
    f (a', _, _) (b', _, _) = compare a' b'
    getParJ (R l) = getParL l
    getParJ (J _ _ (a, _)) = getParJ a
    getParJ (L _ morphs) = morphs
    getParL [] = []
    getParL (p:_) = getParJ p
    calcS :: Morph -> Morph -> Float
    calcS a1 b1 = (score a1) * (score b1)

lexTest :: [GRAM] -> Morph -> Bool
lexTest [] _ = True
lexTest sub morph = rc
  where
    TagSet ts = tag morph
    rc = all (\e -> Set.member e ts) sub

isVerb :: Morph -> Bool
isVerb = lexTest [VERB]

isSubj :: Morph -> Bool
isSubj v = isNounNomn v || isPronoun v

isNounNomn :: Morph -> Bool
isNounNomn n = isNoun n && isNomn n

isNomn :: Morph -> Bool
isNomn n = lexTest [NOMN] n

isNoun :: Morph -> Bool
isNoun a = lexTest [NOUN] a


isAdj:: Morph -> Bool
isAdj a = lexTest [ADJF] a

adjNounConsist :: Morph -> Morph -> Bool
adjNounConsist adj noun = sameDeclination adj noun


nounCases :: Set.Set GRAM
nounCases = Set.fromList
            [NOMN, GENT, DATV,
              ACCS, ABLT, LOCT,
              VOCT, GEN2, ACC2,
              LOC2]

mult :: Set.Set GRAM
mult = Set.fromList [SING, PLUR]

data POSAttr = PMULT | PCASE | PPRON

getProp :: [POSAttr] -> Morph -> Set.Set GRAM
getProp [PCASE] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e nounCases
getProp [PMULT] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e mult
getProp [PPRON] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e pronouns
getProp [] _ = Set.empty:: Set.Set GRAM
getProp (a:t) ts = getProp [a] ts `Set.union` getProp t ts

unTS :: TagSet -> Set.Set GRAM
unTS (TagSet t) = t

sameDeclination :: Morph -> Morph -> Bool
sameDeclination a b = sa =*= sb
  where
    as = [PCASE, PMULT]
    sa = getProp as a
    sb = getProp as b

(=*=) :: Set.Set GRAM -> Set.Set GRAM -> Bool
a =*= b = Set.isSubsetOf a b && Set.isSubsetOf b a

pronouns :: Set.Set GRAM
pronouns = Set.fromList [PER1, PER1, PER3]

isPronoun :: Morph -> Bool
isPronoun p = any f $ Set.toList pronouns
  where
    f pr = lexTest [pr] p

subjVerbConsist :: Morph -> Morph -> Bool
subjVerbConsist pr verb = cpr && cverb
  where
    pra = [PPRON, PMULT]
    cpr = True
    cverb = True
