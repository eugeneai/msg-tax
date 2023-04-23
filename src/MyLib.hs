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
import Data.Char (isPunctuation)

data Subject = Subject {
  name:: T.Text,
  tax:: T.Text
  } deriving (Show, Generic)

data Morph = Morph {
    norm:: T.Text
  , tag:: [GRAM]
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

instance FromJSON GRAM where
  parseJSON :: Value -> Parser GRAM
  parseJSON (String s) = CA.pure $ readGram s
  parseJSON _ = CA.empty

readGram :: T.Text -> GRAM
readGram t =
  let rm = TR.readMaybe . T.unpack . T.toUpper .
           T.replace (T.pack "-") (T.pack "_") $ t :: Maybe GRAM
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
            Join   -- reference to the main grammeme or W
            Morph  -- Morpheme
          deriving (Eq)

instance Show Gram where
  show :: Gram -> String
  show (G g t j m) = "\n(" ++ show g ++ " " ++ show t ++ " " ++ show j ++ " / " ++ show m ++ ")"

data Join = None
          | Wall
          | J GRAM Gram Float
          deriving (Eq)

instance Show Join where
  show :: Join -> String
  show Wall = "⊢"
  show None = "⊥"
  show (J g gr sc) = "〈" ++ show g ++ "→" ++ show morph ++ " " ++ show sc ++ "〉"
    where
      (G _ _ _ morph) = gr

recognize :: [[Gram]] -> [Lex] -> [[Gram]]
recognize grams [] = grams
recognize grams (lex:ls) =
  let grams1 = recognizeOneLex grams lex
  in recognize grams1 ls

recognizeOneLex :: [[Gram]] -> Lex -> [[Gram]]
recognizeOneLex
  prevGrams
  lex = if L.null rc then nrc else rc
  where
    rc = [gram |
           prevGram <- prevGrams,
           currGram <- lexGrams lex,
           gram <- merge prevGram currGram]
    nrc = [gram |
           prevGram <- prevGrams,
           currGram <- lexGrams lex,
           gram <- primMerge prevGram currGram]

    lexGrams :: Lex -> [Gram]
    lexGrams lex
      | L.null . morph $ lex = [G (ucto lex) (w lex) None (Morph (w lex) [(ucto lex)] 1.0)]
      | otherwise = L.sortBy sortf [G aGram (w lex) None m |
                      m <- morph lex,
                      aGram <- possGrams (ucto lex) m]
    sortf
      (G _ _ _ (Morph _ _ s1))
      (G _ _ _ (Morph _ _ s2)) = compare s2 s1 -- decreasing order

    possGrams WORD morph = Set.toList $ getProp [POST] morph
    possGrams WORD_COMPOUND morph = Set.toList $ getProp [POST] morph
      -- Set.toList (maybe (Set.empty) id (M.lookup POST grams))
    possGrams uctoGram _ = [uctoGram]

    primMerge :: [Gram] -> Gram -> [[Gram]]
    primMerge [] (G g t _ m) = [[(G g t Wall m)]]
    primMerge (last:ps) newGram =
      case last of
        (G _ _ (J Sentence _ _) _ ) ->
          let (G g t _ m) = newGram
          in [(G g t Wall m):last:ps]
        _ -> [g:last:ps | g <- noGrammar last newGram]

    noGrammar :: Gram -> Gram -> [Gram]
    noGrammar prev curr =
      let (G gram w _ m) = curr
          gg = [ (G gram w (J NONE prev (score m)) m) ]
      in gg

    merge :: [Gram] -> Gram -> [[Gram]]
    merge [] (G g t _ m) = [[(G g t Wall m)]]

    merge (last:ps) newGram =
      case last of
        (G _ _ (J Sentence _ _) _ ) ->
          let (G g t _ m) = newGram
          in [(G g t Wall m):last:ps]
        _ -> [g:last:ps | g <- grammar last newGram]

    grammar :: Gram -> Gram -> [Gram]
    grammar prev curr =
      let (G jgram pw _ pm) = prev
          (G gram w _ m) = curr
          gg = [ (G gram w (J res ref (jscore pm m)) m) |
                 rule <- maybe [NONE] (Set.toList) (M.lookup JOIN grams),
                 (res, ref) <- applyRule rule prev curr ]
      in gg

    jscore a b = (score a) * (score b)

    applyRule :: Rule r => r -> Gram -> Gram -> [(r, Gram)]
    applyRule rule l r = if left l && right r && both l r
                         then [(rule, l)] else chain l r
      where
        (both, left, right) = join rule
        chain (G g w Wall m) r = []
        chain (G g w None m) r = []
        chain (G g w (J gr g' _) _) r = applyRule rule g' r

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
  | PhoneNumber | VerbTranObjAccs | Sentence | AdvbVerb
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
                        , PhoneNumber, NumrNoun, VerbTranObjAccs
                        , Sentence, AdvbVerb])
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
  join Sentence = (isAnyRel, hasWall, isSentenceEnd)
  join AdvbVerb = (isAnyRel, isVerb, isAdvb)
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

isAdvb :: Gram -> Bool
isAdvb (G ADVB _ _ _) = True
isAdvb _ = False

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

isSentenceEnd :: Gram -> Bool
isSentenceEnd (G PUNCTUATION ch _ _) = L.elem (L.head (T.unpack ch)) (".!?"::String)
isSentenceEnd _ = False

isWord :: String -> Gram -> Bool
isWord word (G _ w _ _ ) = w == T.pack word

hasWall :: Gram -> Bool
hasWall (G _ _ Wall _) = True
hasWall _ = False

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
    Just gramset ->
      let f e = Set.member e gramset
      in Set.fromList . L.filter f . tag $ m
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
    consistency = npr == vpr


-- Use it for checking non working relations

isAnyRel :: Gram -> Gram -> Bool
isAnyRel _ _ = True

isAny :: Gram -> Bool
isAny _ = True

isGram :: GRAM -> Morph -> Bool
isGram gram = lexTest [gram]


-- https://reshutest.ru/theory/8?theory_id=120
