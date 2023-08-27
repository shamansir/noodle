module Main where

import Prelude

import Data.Foldable (fold)
import Data.Tuple.Nested ((/\), type (/\))
import Data.List.Types (NonEmptyList)
import Data.Array (fromFoldable) as Array
import Effect (Effect)
-- import TryPureScript (h1, h3, p, text, render, code)
import Parsing (Parser, runParser)
import Parsing.String (char, string)
import Parsing.Combinators (many1, sepBy1, try)
import Parsing.String.Basic
import Control.Alt ((<|>))
import Data.Array (many)

data Test
  = F Int
  | G String
  | B Boolean
  | N Number



testParserFn
  :: forall x a b c d
   . (String /\ (a -> x) /\ Parser String a)
  -> (String /\ (b -> x) /\ Parser String b)
  -> (String /\ (c -> x) /\ Parser String c)
  -> (String /\ (d -> x) /\ Parser String d)
  -> Parser String x
testParserFn
  (mkA /\ fA /\ parserA)
  (mkB /\ fB /\ parserB)
  (mkC /\ fC /\ parserC)
  (mkD /\ fD /\ parserD)
  =   fA <$> (string mkA *> many1 space *> parserA)
  <|> fB <$> (string mkB *> many1 space *> parserB)
  <|> fC <$> (string mkC *> many1 space *> parserC)
  <|> fD <$> (string mkD *> many1 space *> parserD)


boolParse :: Parser String Boolean
boolParse =
  (string "false" <#> const false)
  <|> (string "true" <#> const true)


testParser :: Parser String (Array Test)
testParser =
  Array.fromFoldable
    <$> (flip sepBy1 (string "\n")
     $  testParserFn
          ("F" /\ F /\ intDecimal)
          ("G" /\ G /\ string "foo")
          ("B" /\ B /\ boolParse)
          ("N" /\ N /\ number))


testString = "F 20\nB true\nG foo\nN 20.0"


instance Show Test where
  show = case _ of
    F n -> "F " <> show n
    G n -> "G " <> show n
    B n -> "B " <> show n
    N n -> "N " <> show n


{-
main :: Effect Unit
main =
  render $ fold
    [ h1 $ text "Examples for purescript-parsing Quick Start"

    , h3 $ code $ text $ show $ runParser testString testParser

    , h3 $ code $ text $ show $ runParser (testString <> testString <> testString) (many testParser)
    ]
-}