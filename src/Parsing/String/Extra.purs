module Parsing.String.Extra where

import Prelude

import Control.Alt ((<|>))

import Data.Semigroup.Foldable (class Foldable1)
import Data.List (List)
import Data.String.CodeUnits as CU
import Data.String.NonEmpty.CodeUnits as NECU
import Data.String.NonEmpty.Internal as StringX
import Data.Array (fromFoldable) as Array
import Data.Array (some) as P

import Parsing (Parser) as P
import Parsing.String (char)  as P
import Parsing.String.Basic (noneOf) as P
import Parsing.Combinators (many1, many1Till) as P
import Parsing.Token (alphaNum) as P


alphaNumToken :: P.Parser String String
alphaNumToken = CU.fromCharArray <$> P.some P.alphaNum


asArray :: forall s a. P.Parser s (List a) -> P.Parser s (Array a)
asArray = map Array.fromFoldable


eol :: P.Parser String Unit
eol = P.char '\n' *> pure unit


anythingBut :: Char -> P.Parser String String
anythingBut char = CU.fromCharArray <$> P.some (P.noneOf [ char ])


tokenChar :: P.Parser String Char
tokenChar = P.alphaNum <|> P.char '-'


commentChar :: P.Parser String Char
commentChar = P.alphaNum <|> P.char '-'


token :: P.Parser String String
token =
  f1ts <$> P.many1 tokenChar
  where
    f1ts :: forall f. Foldable1 f => f Char -> String
    f1ts = NECU.fromFoldable1 >>> StringX.toString


tokenTill :: forall a. P.Parser String a -> P.Parser String String
tokenTill stopAt =
  f1ts <$> P.many1Till tokenChar stopAt
  where
    f1ts :: forall f. Foldable1 f => f Char -> String
    f1ts = NECU.fromFoldable1 >>> StringX.toString