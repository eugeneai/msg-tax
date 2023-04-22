{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib (
    Subject
  , Morph
  , Lex
  , Message
  , translateContent
  , translateLexs
  -- , toText
  -- , toNorm
  -- , convertMsg
  -- , toJoin
  -- , lexsToJoin
  -- , joinPass
  , recognize
  , GRAM (..)
  , Join (..)
  , Gram (..)
  , join
  , text
  , version) where

import Prelude.Compat
    ( otherwise,
      map,
      ($),
      Eq((==)),
      Ord((<=), compare),
      Read,
      Show(show),
      Bool(..),
      String,
      Float,
      Int,
      Maybe(..),
      (.),
      (<$>),
      (<*>),
      all,
      (||),
      (&&) )
-- import BasePrelude (intercalate)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map.Lazy as M
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
    ( Value(String, Object),
      FromJSON(parseJSON),
      ToJSON(toJSON),
      decode,
      (.:) )
-- import Data.Aeson.Types
-- import Data.Text
-- import qualified Text.Show.Unicode as US
import Data.Aeson.Types (
  Parser,
  Array)

data Subject = Subject {
  name:: T.Text,
  tax:: T.Text
  } deriving (Show, Generic)

data Morph = Morph {
    norm:: T.Text
  , tag:: Set.Set GRAM
  , score:: Float
  } deriving (Show, Eq, Generic)

data Lex = Lex {
    w:: T.Text
  , ucto:: GRAM
  , morph:: [Morph]
  } deriving (Show, Generic)

data Message = Message {
  text:: [Lex]
  , subjects:: [Subject]
  } deriving (Show, Generic)


instance FromJSON Subject
instance FromJSON Message
instance FromJSON Lex

instance FromJSON Morph

-- toTagSet :: Parser Morph -> Set.Set GRAM
-- toTagSet (Parser m) = myUnpack m
--   where
--     -- myUnpack :: [Value] -> Set.Set g
--     myUnpack values = Set.fromList $ concatMap val values
--     -- val :: Value -> [GRAM]
--     val (String s) = [readGram s]
--     -- val val = concatMap val . V.toList $ a
--     -- val _ = [BAD]

instance FromJSON GRAM where
  parseJSON :: Value -> Parser GRAM
  parseJSON (String s) = CA.pure $ readGram s
  parseJSON _ = CA.empty

readGram :: T.Text -> GRAM
readGram t =
  let rm =TR.readMaybe . T.unpack . T.toUpper. T.replace (T.pack "-") (T.pack "_") $ t :: Maybe GRAM
  in
    case rm of
      Nothing -> case T.unpack . T.toUpper $ t of
        "1PER" -> PER1
        "2PER" -> PER3
        "3PER" -> PER3
        _ -> UNKNOWN
      Just g -> g :: GRAM

instance ToJSON Subject
instance ToJSON Message
instance ToJSON Lex
instance ToJSON Morph

-- instance ToJSON (Set.Set GRAM) where
--   toJSON s = Array . V.fromList . map f . Set.toList $ s
--     where
--       f gram = String . T.pack . show $ gram


instance ToJSON GRAM where
  toJSON :: GRAM -> Value
  toJSON u = String . T.pack . show $ u

translateContent :: BL.ByteString -> Maybe Message
translateContent content = do
  let obj = decode content :: Maybe Message
  obj

translateLexs :: BL.ByteString -> Maybe [Lex]
translateLexs content = do
  let obj = decode content :: Maybe [Lex]
  obj

defNoParse :: T.Text
defNoParse = (T.pack "Error: no parse")

-- toText :: Maybe Message -> T.Text
-- toText = convertMsg defNoParse w

-- toNorm :: Maybe Message -> T.Text
-- toNorm = convertMsg defNoParse c
--   where
--     c lex = go morphs
--     morphs = morph lex
--     go :: [Morph] -> T.Text
--     go xs = T.intercalate "/" . map gonorm $ xs :: T.Text
--     gonorm :: Morph -> T.Text
--     gonorm = norm

convertMsg :: T.Text -> (Lex -> T.Text) -> Maybe Message -> T.Text
convertMsg def _ Nothing = def
convertMsg _ c (Just msg) = T.intercalate (T.pack " ") . map c $ text msg

version :: String
version = "0.0.1"

data Gram = G GRAM
            T.Text
            Join   -- reference to the main grammeme
            Morph  -- Morpheme
          deriving (Eq, Show)

data Join = W -- Wall
          | J GRAM Gram Float
          deriving (Eq, Show)

recognize :: [Lex] -> [[Join]]
recognize lexs = [[W]]

-- instance Show Join where
--   show (J g a b s) = "(J " ++ show s ++ " " ++ show c ++ " " ++ tails ++ ")\n"
--     where
--       tails = show a ++ " " ++ show b


-- lexsToJoin [] = W
-- lexsToJoin (lex:lexs) = lext lex prev
--   where
--     prev = lexsToJoin lexs
--     lext lex prev =
--       let rc = go lex
--       in if L.null rc then [J (ucto lex) (w lex) W (Morph (w lex) Set.empty 1.0)]
--          else rc

--     go l = [ P gram (w l) (conv gram m) |
--              gram <- Set.toList $ (maybe (Set.empty) id (M.lookup POST grams)),
--              m <- (morphs l),
--              lexTest [gram] m]

--     conv gram m =
--       let (Morph w ts score) = m
--       in (Morph w (Set.delete gram ts) score)
--     morphs l = case morph l of
--                Nothing -> []
--                Just m -> m

-- toJoin :: Maybe Message -> Maybe [[Join]]
-- toJoin Nothing = Nothing
-- toJoin (Just msg) = lexsToJoin . text $ msg

class Rule r where
  join :: r -> (Gram->Gram->Bool, Gram->Bool, Gram->Bool)
  join3 :: (r, String) -> (Gram->Gram->Bool, Gram->Bool, Gram->Bool)

scoreSort :: [(Float, a, a)] -> [(Float, a, a)]
scoreSort l = L.sortBy f l
  where
    f (a', _, _) (b', _, _) = compare b' a' -- descending


data GRAM = BAD
  | POST  | NOUN  | ADJF  | ADJS  | COMP  | VERB  | INFN  | PRTF  | PRTS
  | GRND  | NUMR  | ADVB  | NPRO  | PRED  | PREP  | CONJ  | PRCL  | INTJ  | ANIM
  | INAN  | GNDR  | MASC  | FEMN  | NEUT  | MS_F  | NMBR  | SING  | PLUR  | SGTM
  | PLTM  | FIXD  | CASE  | NOMN  | GENT  | DATV  | ACCS  | ABLT  | LOCT  | VOCT
  | GEN1  | GEN2  | ACC2  | LOC1  | LOC2  | ABBR  | NAME  | SURN  | PATR  | GEOX
  | ORGN  | TRAD  | SUBX  | SUPR  | QUAL  | APRO  | ANUM  | POSS  | V_EY  | V_OY
  | CMP2  | V_EJ  | ASPC  | PERF  | IMPF  | TRNS  | TRAN  | INTR  | IMPE  | IMPX
  | MULT  | REFL  | PERS  | PER1  | PER2  | PER3  | TENS  | PRES  | PAST  | FUTR
  | MOOD  | INDC  | IMPR  | INVL  | INCL  | EXCL  | VOIC  | ACTV  | PSSV  | INFR
  | SLNG  | ARCH  | LITR  | ERRO  | DIST  | QUES  | DMNS  | PRNT  | V_BE  | V_EN
  | V_IE  | V_BI  | FIMP  | PRDX  | COUN  | COLL  | V_SH  | AF_P  | INMX  | VPRE
  | ANPH  | INIT  | ADJX  | HYPO  | LATN  | UNKN
  | NONE  -- No reference
--  | Unrec T.Text -- For all unknown
  | UCTO
  | URL | URL_WWW | URL_DOMAIN -- Ucto
  | E_MAIL | ABBREVIATION_KNOWN | WORD_PARPREFIX
  | WORD_PARSUFFIX | WORD_COMPOUND
  | ABBREVIATION | INITIAL | SMILEY | REVERSE_SMILEY
  | PUNCTUATION_MULTI | DATE_REVERSE | DATE
  | NUMBER_YEAR | TIME | FRACNUMBER | NUMBER
  | CURRENCY | WORD | PUNCTUATION | UNKNOWN
  | JOIN
  | AdjNoun | NumrNoun | SubjVerb | NounNounGent | Percent
  | PhoneNumber | VerbTranObjAccs
  | JOIN3
  | ForJoin | NounInNoun
  deriving (Show, Read, Ord, Eq)


grams :: M.Map GRAM (Set.Set GRAM)
grams = M.fromList [
    (POST, Set.fromList [NOUN, ADJF, ADJS, COMP, VERB, INFN,
                         PRTF, PRTS, GRND, NUMR, ADVB, NPRO,
                         PRED, PREP, CONJ, PRCL, INTJ])
  , (ANIM, Set.fromList [ANIM, INAN])
  , (MS_F, Set.fromList [MASC, FEMN])
  , (GNDR, Set.fromList [NEUT, MS_F])
  , (NMBR, Set.fromList [SING, PLUR])
  , (CASE, Set.fromList [NOMN, GENT, DATV, ACCS, ABLT,
                         LOCT, VOCT, GEN2, ACC2, LOC2])
  , (GENT, Set.fromList [GEN1, GEN2])
  , (ACCS, Set.fromList [ACC2])
  , (LOCT, Set.fromList [LOC1, LOC2])
  , (ASPC, Set.fromList [PERF, IMPF])
  , (TRNS, Set.fromList [TRAN, INTR])
  , (PERS, Set.fromList [PER1, PER2, PER3, NPRO])
  , (TENS, Set.fromList [PRES, PAST, FUTR])
  , (MOOD, Set.fromList [INDC, IMPR])
  , (INVL, Set.fromList [INCL, EXCL])
  , (VOIC, Set.fromList [ACTV, PSSV])
  , (UCTO, Set.fromList [URL, URL_WWW, URL_DOMAIN
                        , E_MAIL, ABBREVIATION_KNOWN, WORD_PARPREFIX
                        , WORD_PARSUFFIX, WORD_COMPOUND
                        , ABBREVIATION, INITIAL, SMILEY, REVERSE_SMILEY
                        , PUNCTUATION_MULTI, DATE_REVERSE, DATE
                        , NUMBER_YEAR, TIME, FRACNUMBER, NUMBER
                        , CURRENCY, WORD, PUNCTUATION, UNKNOWN])
  , (JOIN, Set.fromList [AdjNoun, SubjVerb, NounNounGent, Percent
                        , PhoneNumber, NumrNoun, VerbTranObjAccs ])
  , (JOIN3, Set.fromList [ForJoin, NounInNoun])
  ]



-- Simple Grammar rules

instance Rule GRAM where

  join :: GRAM -> (Gram -> Gram -> Bool, Gram -> Bool, Gram -> Bool)
  join AdjNoun = (adjNounConsist, isAdj, isNoun)
  join NumrNoun = (numrNounConsist, isNumr, isNoun)
  join SubjVerb = (subjVerbConsist, isSubj, isVerb)
  join VerbTranObjAccs = (isAnyRel, isVerbTran, isObjAccs)
  join NounNounGent = (isAnyRel, isNoun, isNounGent)
  join Percent = (isAnyRel, isNum100, isPercent)
  join PhoneNumber = (isAnyRel, isWord "+", isPhoneNumber)
  join _ = (lfm, lf, lf)
    where lf _ = False
          lfm _ _ = False

  join3 :: (GRAM, String) -> (Gram -> Gram -> Bool, Gram -> Bool, Gram -> Bool)
  join3 (ForJoin, "для") = (isAnyRel, isNoun, isNounGent)
  join3 (NounInNoun, "в") = (isAnyRel, isNoun, isNounLoct)
  join3 _ = (lfm, lf, lf)
    where lf _ = False
          lfm _ _ = False

minimumScore :: Float
minimumScore = 0 -- 1e-4
topN :: Int
topN = 20

data RelSide = LeftSide | RightSide
  deriving (Show, Eq)

lexTest :: [GRAM] -> Morph -> Bool
lexTest [] _ = True
lexTest sub morph = rc
  where
    ts = tag morph
    rc = all (\e -> L.elem e ts) sub

isVerb :: Gram -> Bool
isVerb (G VERB _ _ _) = True
isVerb _ = False

isVerbTran :: Gram -> Bool
isVerbTran (G VERB _ _ m) = lexTest[TRAN] m
isVerbTran _ = False

isSubj :: Gram -> Bool
isSubj v = isNounNomn v || isPronoun v

isObjAccs :: Gram -> Bool
isObjAccs v = (isNoun v || isPronoun v) && isAccs v

isNounNomn :: Gram -> Bool
isNounNomn n = isNoun n && isNomn n

isNounGent :: Gram -> Bool
isNounGent n = isNoun n && isGent n

isNounLoct :: Gram -> Bool
isNounLoct n = isNoun n && isLoct n

isNomn :: Gram -> Bool
isNomn (G pos _ _ m) = lexTest [NOMN] m

isGent :: Gram -> Bool
isGent (G pos _ _ m) = lexTest [GENT] m

isLoct :: Gram -> Bool
isLoct (G pos _ _ m) = lexTest [LOCT] m

isAccs :: Gram -> Bool
isAccs (G pos _ _ m) = lexTest [ACCS] m

isNoun :: Gram -> Bool
isNoun (G NOUN _ _ _) = True
isNoun _ = False

isAdj:: Gram -> Bool
isAdj (G ADJF _ _ _) = True
isAdj _ = False

isNumr :: Gram -> Bool
isNumr (G NUMR _ _ _) = True
isNumr _ =False

isNum100 :: Gram -> Bool
isNum100 (G NUMBER w _ _) =
  case (TR.readMaybe (T.unpack w))::Maybe Int of
    Nothing -> False
    Just n -> n <= 100
isNum100 _ = False

isNumber :: Gram -> Bool
isNumber (G NUMBER w _ _) =
  case (TR.readMaybe (T.unpack w))::Maybe Int of
    Nothing -> False
    Just _ -> True
isNumber _ = False

isPhoneNumber :: Gram -> Bool
isPhoneNumber l = isNumber l && hasLength 11 l

hasLength :: Int -> Gram -> Bool
hasLength l (G _ w _ _) = T.length w == l

isPercent :: Gram -> Bool
isPercent (G PUNCTUATION percent _ _) = percent == T.pack "%"
isPercent _ = False

isWord :: String -> Gram -> Bool
isWord word (G _ w _ _ ) = w == T.pack word


adjNounConsist :: Gram -> Gram -> Bool
adjNounConsist adj noun = sameDeclination adj noun

numrNounConsist :: Gram -> Gram -> Bool
numrNounConsist (G _ _ _ a) (G _ _ _ b) = sa =*= sb
  where
    as = [CASE]
    sa = getProp as a
    sb = getProp as b

getProp :: [GRAM] -> Morph -> Set.Set GRAM
getProp [cls] m =
  case set of
    Nothing -> Set.empty
    Just s ->
      let f e = Set.member e s
      in Set.filter f . tag $ m
  where
    set = M.lookup cls grams

getProp [] _ = Set.empty:: Set.Set GRAM
getProp (a:t) ts = getProp [a] ts `Set.union` getProp t ts

sameDeclination :: Gram -> Gram -> Bool
sameDeclination (G _ _ _ a) (G _ _ _ b) = sa =*= sb
  where
    as = [CASE, NMBR]
    sa = getProp as a
    sb = getProp as b

(=*=) :: Set.Set GRAM -> Set.Set GRAM -> Bool
a =*= b = Set.isSubsetOf a b && Set.isSubsetOf b a

pronouns :: Set.Set GRAM
pronouns = Set.fromList [PER1, PER2, PER3, NPRO]

isPronoun :: Gram -> Bool
isPronoun (G pr _ _ _) = Set.member pr pronouns
isPronoun _ = False

subjVerbConsist :: Gram -> Gram -> Bool
subjVerbConsist (G subj wsubj j msubj) (G VERB verb _ mverb)
  | isPronoun (G subj wsubj j msubj) = consistency
  | subj == NOUN = consistency
  | otherwise = False
  where
    pna = [NMBR]
    vpr = getProp pna mverb
    npr = getProp pna msubj
    consistency = npr =*= vpr


-- Use it for checking non working relations

isAnyRel :: Gram -> Gram -> Bool
isAnyRel _ _ = True

isAny :: Gram -> Bool
isAny _ = True

isGram :: GRAM -> Morph -> Bool
isGram gram = lexTest [gram]


-- https://reshutest.ru/theory/8?theory_id=120
