module Parsing.HasParser where

import Prelude

import Parsing (Parser, runParser)

import Data.Maybe (Maybe)
import Data.Either (hush) as Either


class HasParser x where
    parser :: Parser String x


read :: forall x. HasParser x => String -> Maybe x
read = flip runParser (parser :: Parser String x) >>> Either.hush