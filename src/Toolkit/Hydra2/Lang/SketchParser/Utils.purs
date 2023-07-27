module Toolkit.Hydra2.Lang.SketchParser.Utils where

import Prelude


import Data.Semigroup.Foldable (class Foldable1)
import Data.CodePoint.Unicode as U
import Data.String.CodePoints (codePointFromChar)

import Parsing (Parser)
import Parsing.String (char, satisfy)
import Parsing.String.Basic (alphaNum, space)
import Parsing.Combinators (many1, many1Till)

import Data.Array (many)
import Data.List.NonEmpty (NonEmptyList)
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX


betweenSpaces :: forall a. Parser String a -> Parser String a
betweenSpaces parser = do
    -- between (many space) (many space)
    _ <- spaces
    p <- parser
    _ <- spaces
    pure p


tokenChar :: Parser String Char
tokenChar = alphaNum


spaceOrEol :: Parser String Char
spaceOrEol = satisfy $ \c -> U.isSpace (codePointFromChar c) || (c == '\n') || (c == '\t')


f1ts :: forall f. Foldable1 f => f Char -> String
f1ts = CU.fromFoldable1 >>> StringX.toString


tokenTill :: forall a. Parser String a -> Parser String (NonEmptyList Char)
tokenTill stopAt =
  many1Till tokenChar stopAt


eol :: Parser String Unit
eol = char '\n' *> pure unit


spaces :: Parser String (Array Char)
spaces = many space


breaksAndSpaces :: Parser String (Array Char)
breaksAndSpaces = many spaceOrEol


token :: Parser String (NonEmptyList Char)
token = many1 tokenChar


maybeToken :: Parser String (Array Char)
maybeToken = many tokenChar
