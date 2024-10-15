module Parsing.String.Extra where

import Prelude

import Control.Alt ((<|>))

import Data.Semigroup.Foldable (class Foldable1)
import Data.List (List)
import Data.String.CodeUnits as CU
import Data.String.NonEmpty.CodeUnits as NECU
import Data.String.NonEmpty.Internal as StringX
import Data.String.CodePoints (codePointFromChar) as CP
import Data.CodePoint.Unicode as CPU
import Data.Array (fromFoldable) as Array
import Data.Array (some, many) as P

import Parsing (Parser) as P
import Parsing.String (char, satisfy)  as P
import Parsing.String.Basic (noneOf) as P
import Parsing.Combinators (many1, many1Till) as P
import Parsing.Token (alphaNum, space) as P


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


tokenTill :: forall a. P.Parser String a -> P.Parser String String
tokenTill stopAt =
  f1ts <$> P.many1Till tokenChar stopAt
betweenSpaces :: forall a. P.Parser String a -> P.Parser String a
betweenSpaces parser = do
    -- between (many space) (many space)
    _ <- spaces
    p <- parser
    _ <- spaces
    pure p


spaceOrEol :: P.Parser String Char
spaceOrEol = P.satisfy $ \c -> CPU.isSpace (CP.codePointFromChar c) || (c == '\n') || (c == '\t')


f1ts :: forall f. Foldable1 f => f Char -> String
f1ts = NECU.fromFoldable1 >>> StringX.toString


spaces :: P.Parser String (Array Char)
spaces = P.many P.space


breaksAndSpaces :: P.Parser String (Array Char)
breaksAndSpaces = P.many spaceOrEol


maybeToken :: P.Parser String (Array Char)
maybeToken = P.many tokenChar
