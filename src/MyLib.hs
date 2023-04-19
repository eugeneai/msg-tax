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
  , toText
  , toNorm
  , convertMsg
  , toJoin
  , joinPass
  , Connection (AdjNoun, SubjVerb, NounNounGent)
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

newtype TagSet = TagSet (Set.Set GRAM)

instance Show TagSet where
  show (TagSet s)
    | Set.null s = "*"
    | otherwise = show . Set.toList $ s

data Morph = Morph {
    norm:: T.Text
  , tag:: TagSet
  , score:: Float
  } deriving (Show, Generic)

data Lex = Lex {
    w:: T.Text
  , ucto:: Ucto
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

instance FromJSON Ucto where
  parseJSON (String s) = CA.pure $ readUcto
    where
      sr = TR.readMaybe . T.unpack . T.toUpper. T.replace (T.pack "-") (T.pack "_") $ s :: Maybe Ucto
      readUcto = case sr of
        Nothing -> UNKNOWN
        Just a -> a
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

  toJSON :: TagSet -> Value
  toJSON (TagSet s) = Array . V.fromList . map f . Set.toList $ s
    where
      f gram = String . T.pack . show $ gram


instance ToJSON Ucto where
  toJSON :: Ucto -> Value
  toJSON u = String . T.pack . show $ u

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

data Connection = AdjNoun | SubjVerb | NounNounGent
  deriving (Show, Eq, Read)

data Ucto = URL | URL_WWW | URL_DOMAIN
  | E_MAIL | ABBREVIATION_KNOWN | WORD_PARPREFIX
  | WORD_PARSUFFIX | WORD_COMPOUND
  | ABBREVIATION | INITIAL | SMILEY | REVERSE_SMILEY
  | PUNCTUATION_MULTI | DATE_REVERSE | DATE
  | NUMBER_YEAR | TIME | FRACNUMBER | NUMBER
  | CURRENCY | WORD | PUNCTUATION | UNKNOWN
  deriving (Show, Eq, Read)

data Join = R [Join] | J Connection Join Join | M Ucto T.Text [Morph]

instance Show Join where
  show (J c a b) = "(J " ++ show c ++ " " ++ tails ++ ")\n"
    where
      tails = show a ++ " " ++ show b
  show (R ts) = "(R " ++ (concatMap show $ ts) ++ ")"
  show (M u w ts) = "|" ++ show u ++ " " ++ show w ++ " [ " ++ show ts ++ "]\n"

toJoin :: Maybe Message -> Maybe Join
toJoin Nothing = Nothing
toJoin (Just msg) = Just . R . map lext . text $ msg
  where
    lext l = M (ucto l) (w l) (maybe [] id $ morph l)

class Rule r where
  join :: r -> (Morph->Morph->Bool, Morph->Bool, Morph->Bool)

joinPass :: Connection -> Join -> Join
joinPass r (R l) = R $ applyRule r l
joinPass _ rest = rest

applyRule :: Connection -> [Join] -> [Join]
applyRule _ [] = []
applyRule _ [e] = [e]
applyRule r (a:b:l) = rc
  where
    (mjoin, left, right) = join r
    lm = [ (ma, mb) | (ma, mb) <- compPairs (left, right) a b, mjoin ma mb ]
    morphs = head lm
    nj = red (a, b) morphs
    rc = if null lm then a:(applyRule r (b:l))
         else applyRule r (nj:l)
    red :: (Join, Join) -> (Morph, Morph) -> Join
    red (M u w _, M u' w' _) (ma, mb)  = J r (M u w [ma]) (M u' w' [mb])
    red (M u w _, J c a b)   (ma, _)   = J r (M u w [ma]) (J c a b)
    red (J c a b, M u w _)   (_,  mb)  = J r (J c a b)    (M u w [mb])
    red (a,       b)         (_,  _)   = J r a b

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
  | PRONOUN
  | Unrec T.Text -- For all unknown
  deriving (Show, Read, Ord, Eq)

-- Simple Grammar rules

instance Rule Connection where
  join :: Connection -> (Morph -> Morph -> Bool, Morph -> Bool, Morph -> Bool)
  join AdjNoun = (adjNounConsist, isAdj, isNoun)
  join SubjVerb = (subjVerbConsist, isSubj, isVerb)
  join NounNounGent = (isAnyRel, isNoun, isNounGent)
  -- join _ = (lfm, lf, lf)
  --   where
  --     lf _ = False
  --     lfm _ _ = False

minimumScore :: Float
minimumScore = 0.01

data RelSide = LeftSide | RightSide
  deriving (Show, Eq)

compPairs :: (Morph->Bool, Morph->Bool) -> Join -> Join -> [(Morph, Morph)]
compPairs (left, right) a b = rc
  where
    rc = L.map g . L.sortBy f $ ([ (calcS aa bb, aa, bb) |
                                   aa <- getParJ LeftSide a, left aa,
                                   bb <- getParJ RightSide b, right bb,
                                  calcS aa bb > minimumScore])
    g (_, b', c') = (b',c')
    f :: (Float, Morph, Morph) -> (Float, Morph, Morph) -> Ordering
    f (a', _, _) (b', _, _) = compare a' b'
    getParJ s (R l) = getParL s l
    getParJ LeftSide (J _ aj bj) = getParJ LeftSide aj
    getParJ RightSide (J _ aj bj) = getParJ RightSide bj
    getParJ _ (M _ _ morphs) = morphs
    getParL _ [] = []
    getParL s (p:_) = getParJ s p

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

isNounGent :: Morph -> Bool
isNounGent n = isNoun n && isGent n

isNomn :: Morph -> Bool
isNomn = lexTest [NOMN]

isGent :: Morph -> Bool
isGent = lexTest [GENT]

isNoun :: Morph -> Bool
isNoun = lexTest [NOUN]


isAdj:: Morph -> Bool
isAdj = lexTest [ADJF]

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

getProp :: [GRAM] -> Morph -> Set.Set GRAM
getProp [CASE] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e nounCases
getProp [MULT] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e mult
getProp [PRONOUN] m = Set.filter f $ unTS . tag $ m
  where f e = Set.member e pronouns
getProp [] _ = Set.empty:: Set.Set GRAM
getProp (a:t) ts = getProp [a] ts `Set.union` getProp t ts

unTS :: TagSet -> Set.Set GRAM
unTS (TagSet t) = t

sameDeclination :: Morph -> Morph -> Bool
sameDeclination a b = sa =*= sb
  where
    as = [CASE, MULT]
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
subjVerbConsist subj verb = pronVerb || nounVerb
  where
    pra = [PRONOUN, MULT]
    va1 = [PRONOUN, MULT]
    va2 = [MULT]
    na = [MULT]
    ppr = getProp pra subj
    vpr1 = getProp va1 verb
    vpr2 = getProp va2 verb
    npr = getProp na subj
    pronVerb = ppr =*= vpr1
    nounVerb = npr =*= vpr2


-- Use it for checking non working relations

isAnyRel :: Morph -> Morph -> Bool
isAnyRel _ _ = True

isAny :: Morph -> Bool
isAny _ = True
