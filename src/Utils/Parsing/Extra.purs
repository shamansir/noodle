module Parsing.Extra where

import Prelude

import Control.Plus (empty)

import Data.Foldable (foldr)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (fromFoldable) as Array

import Parsing (Parser, runParser, ParseState(..), getParserT)
import Parsing.String (string)
import Parsing.Combinators (many1, sepBy1)
import Parsing.String.Basic (space)

import Data.Maybe (Maybe)
import Data.Either (hush) as Either

import Control.Alt ((<|>))


class HasParser x where
    parser :: Parser String x


read :: forall x. HasParser x => String -> Maybe x
read = flip runParser (parser :: Parser String x) >>> Either.hush


newtype Marker x = Marker ((forall a. (String /\ (a -> x) /\ Parser String a) -> Parser String x) -> Parser String x)


marker :: forall a x. (String /\ (a -> x) /\ Parser String a) -> Marker x
marker s = Marker \f -> f s


callMarker :: forall x. Marker x -> (forall a. (String /\ (a -> x) /\ Parser String a) -> Parser String x) -> Parser String x
callMarker (Marker f) = f


foldMarkers
  :: forall x
   . Array (Marker x)
  -> Parser String x
foldMarkers =
  foldr ((<|>)) empty <<< map adaptMarker
  where
    adaptMarker :: Marker x -> Parser String x
    adaptMarker sp = callMarker sp adaptParser
    adaptParser :: forall a x'. (String /\ (a -> x') /\ Parser String a) -> Parser String x'
    -- adaptParser :: forall a. String /\ (a -> x) /\ Parser String a -> Parser String x
    adaptParser (mkA /\ fA /\ parserA) = fA <$> (string mkA *> many1 space *> parserA)


parseBy :: forall x. Array (Marker x) -> Parser String (Array x)
parseBy = linesOf <<< foldMarkers



linesOf :: forall x. Parser String x -> Parser String (Array x)
linesOf parser =
  Array.fromFoldable
    <$> (sepBy1 parser $ string "\n")


source :: forall s. Parser s s
source = getParserT >>= \(ParseState src _ _) -> pure src