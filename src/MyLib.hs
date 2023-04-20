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
  , GRAM (AdjNoun, SubjVerb, NounNounGent)
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
-- import Data.Aeson.Types
-- import Data.Text
-- import qualified Text.Show.Unicode as US

data Subject = Subject {
  name:: T.Text,
  tax:: T.Text
  } deriving (Show, Generic)

newtype TagSet = TagSet (Set.Set GRAM)
  deriving (Eq)

instance Show TagSet where
  show (TagSet s)
    | Set.null s = "*"
    | otherwise = show . Set.toList $ s

data Morph = Morph {
    norm:: T.Text
  , tag:: TagSet
  , score:: Float
  } deriving (Show, Generic, Eq)

data Lex = Lex {
    w:: T.Text
  , ucto:: GRAM
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

instance FromJSON GRAM where
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

instance ToJSON TagSet where

  toJSON :: TagSet -> Value
  toJSON (TagSet s) = Array . V.fromList . map f . Set.toList $ s
    where
      f gram = String . T.pack . show $ gram


instance ToJSON GRAM where
  toJSON :: GRAM -> Value
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

-- data Ucto =
--   deriving (Show, Eq, Read)

data Join = J GRAM Join Join Float
          | P GRAM T.Text Morph
          deriving (Eq)

instance Show Join where
  show (J c a b s) = "(J " ++ show s ++ " " ++ show c ++ " " ++ tails ++ ")\n"
    where
      tails = show a ++ " " ++ show b
  show (P u w ts) = "<" ++ show u ++ "> " ++ show w ++ " / " ++ show ts ++ "\n"

toJoin :: Maybe Message -> Maybe [[Join]]
toJoin Nothing = Nothing
toJoin (Just msg) = Just . map lext . text $ msg
  where
    lext lex =
      let rc = go lex
      in if L.null rc then [P (ucto lex) (w lex) (Morph (w lex) (TagSet Set.empty) 1.0)]
         else rc

    go l = [ P gram (w l) (conv gram m) |
             gram <- Set.toList $ (maybe (Set.empty) id (M.lookup POST grams)),
             m <- (morphs l),
             lexTest [gram] m]

    conv gram m =
      let (Morph w (TagSet ts) score) = m
      in (Morph w (TagSet (Set.delete gram ts)) score)
    morphs l = case morph l of
               Nothing -> []
               Just m -> m

class Rule r where
  join :: r -> (Join->Join->Bool, Join->Bool, Join->Bool, Bool)

joinPass :: [[Join]] -> [[Join]]
joinPass l = applyRule l

applyRule :: [[Join]] -> [[Join]]
applyRule [] = []
applyRule [e] = [e]
applyRule (as:bs:l) = rc
  where
    lm = [ (score, ma, mb, r) |
           r <- rules,
           (mjoin, left, right, rev) <- [join r],
           (score, ma, mb) <- compPairs (left, right) as bs rev,
           mjoin ma mb ]

    rules = maybe [] (Set.toList) (M.lookup JOIN grams)

--    f :: (Float, Join, Join) -> (Float, Join, Join) -> Ordering
--    f (a', _, _) (b', _, _) = compare a' b'

    nj = map red lm
    rc = if null lm then as:(applyRule (bs:l))
         else applyRule (nj:l)
    red :: (Float, Join, Join, GRAM) -> Join
    red (s, a, b, r) = J r a b s

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
  | Unrec T.Text -- For all unknown
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
  | PhoneNumber
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
  , (PERS, Set.fromList [PER1, PER2, PER3])
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
                        , PhoneNumber, NumrNoun ])
  ]



-- Simple Grammar rules

instance Rule GRAM where

  join :: GRAM -> (Join -> Join -> Bool, Join -> Bool, Join -> Bool, Bool)
  join AdjNoun = (adjNounConsist, isAdj, isNoun, False)
  join NumrNoun = (numrNounConsist, isNumr, isNoun, True)
  join SubjVerb = (subjVerbConsist, isSubj, isVerb, False)
  join NounNounGent = (isAnyRel, isNoun, isNounGent, False)
  join Percent = (isAnyRel, isNum100, isPercent, False)
  join PhoneNumber = (isAnyRel, isWord "+", isPhoneNumber, False)
  join _ = (lfm, lf, lf, False)
    where lf _ = False
          lfm _ _ = False

minimumScore :: Float
minimumScore = 0.00

data RelSide = LeftSide | RightSide
  deriving (Show, Eq)

compPairs :: (Join->Bool, Join->Bool) -> [Join] -> [Join] -> Bool -> [(Float, Join, Join)]
compPairs (left, right) as bs rev = if rev
                                    then L.sortBy f $ (rc ++ revrc)
                                    else rc
  where
    rc = L.sortBy f $ [ (calcS aa' bb', aa', bb') |
                        aa <- as,
                        aa' <- [getParJ LeftSide aa],
                        left aa',
                        bb <- bs,
                        bb' <- [getParJ RightSide bb],
                        right bb',
                        calcS aa' bb' >= minimumScore]

    revrc = L.sortBy f $ [ (calcS aa' bb', aa', bb') |
                        aa <- bs,
                        aa' <- [getParJ LeftSide aa],
                        left aa',
                        bb <- as,
                        bb' <- [getParJ RightSide bb],
                        right bb',
                        calcS aa' bb' >= minimumScore]
    g (_, b', c') = (b',c')

    f :: (Float, Join, Join) -> (Float, Join, Join) -> Ordering
    f (a', _, _) (b', _, _) = compare b' a' -- descending

    getParJ :: RelSide -> Join -> Join
    getParJ LeftSide (J _ aj _ _) = getParJ LeftSide aj
    getParJ RightSide (J _ _ bj _) = getParJ RightSide bj
    getParJ _ pos = pos

    calcS :: Join -> Join -> Float
    calcS j1 j2 = (sco j1) * (sco j2)
    sco (P _ _ a1) = score a1
    sco (J _ a1 b1 s) = s -- (calcS a1 b1)

lexTest :: [GRAM] -> Morph -> Bool
lexTest [] _ = True
lexTest sub morph = rc
  where
    TagSet ts = tag morph
    rc = all (\e -> Set.member e ts) sub

isVerb :: Join -> Bool
isVerb (P VERB _ _) = True
isVerb _ = False

isSubj :: Join -> Bool
isSubj v = isNounNomn v || isPronoun v

isNounNomn :: Join -> Bool
isNounNomn n = isNoun n && isNomn n

isNounGent :: Join -> Bool
isNounGent n = isNoun n && isGent n

isNomn :: Join -> Bool
isNomn (P pos _ m) = lexTest [NOMN] m
isNomn _ = False

isGent :: Join -> Bool
isGent (P pos _ m) = lexTest [GENT] m
isGent _ = False

isNoun :: Join -> Bool
isNoun (P NOUN  _ _) = True
isNoun _ = False

isAdj:: Join -> Bool
isAdj (P ADJF _ _) = True
isAdj _ = False

isNumr (P NUMR _ _) = True
isNumr _ =False

isNum100 :: Join -> Bool
isNum100 (P NUMBER w _) =
  case (TR.readMaybe (T.unpack w))::Maybe Int of
    Nothing -> False
    Just n -> n <= 100
isNum100 _ = False

isNumber (P NUMBER w _) =
  case (TR.readMaybe (T.unpack w))::Maybe Int of
    Nothing -> False
    Just n -> True
isNumber _ = False

isPhoneNumber l = isNumber l && hasLength 11 l

hasLength l (P _ w _) = T.length w == l
hasLength _ _ = False

isPercent :: Join -> Bool
isPercent (P PUNCTUATION percent _) = percent == T.pack "%"
isPercent _ = False

isWord word (P _ w _ ) = w == T.pack word
isWord _ _ = False


adjNounConsist :: Join -> Join -> Bool
adjNounConsist adj noun = sameDeclination adj noun

numrNounConsist (P _ _ a) (P _ _ b) = sa =*= sb
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
      in Set.filter f $ unTS . tag $ m
  where
    set = M.lookup cls grams

getProp [] _ = Set.empty:: Set.Set GRAM
getProp (a:t) ts = getProp [a] ts `Set.union` getProp t ts

unTS :: TagSet -> Set.Set GRAM
unTS (TagSet t) = t

sameDeclination :: Join -> Join -> Bool
sameDeclination (P _ _ a) (P _ _ b) = sa =*= sb
  where
    as = [CASE, NMBR]
    sa = getProp as a
    sb = getProp as b

(=*=) :: Set.Set GRAM -> Set.Set GRAM -> Bool
a =*= b = Set.isSubsetOf a b && Set.isSubsetOf b a

pronouns :: Set.Set GRAM
pronouns = Set.fromList [PER1, PER2, PER3]

isPronoun :: Join -> Bool
isPronoun (P pr _ _) = Set.member pr pronouns
isPronoun _ = False

subjVerbConsist :: Join -> Join -> Bool
subjVerbConsist (P subj wsubj msubj) (P VERB verb mverb)
  | isPronoun (P subj wsubj msubj) = consistency
  | subj == NOUN = consistency
  | otherwise = False
  where
    pna = [NMBR]
    vpr = getProp pna mverb
    npr = getProp pna msubj
    consistency = npr =*= vpr


-- Use it for checking non working relations

isAnyRel :: Join -> Join -> Bool
isAnyRel _ _ = True

isAny :: Join -> Bool
isAny _ = True

isGram gram = lexTest [gram]
