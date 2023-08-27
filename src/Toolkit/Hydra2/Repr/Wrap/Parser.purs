module Toolkit.Hydra2.Repr.Wrap.Parser where

import Data.FromToFile

import Prelude

import Parsing.Extra (marker, foldMarkers, parseBy)

import Control.Plus (empty)

import Data.Foldable (foldr)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (fromFoldable) as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Parsing (Parser, runParser)
import Parsing.String (string)
import Parsing.Combinators (many1, sepBy1)
import Parsing.String.Basic (space)
import Control.Alt ((<|>))


import Toolkit.Hydra2.Repr.Wrap


-- newtype HasParser = HasParser WrapRepr

class HasParser x where
    parser :: Parser String x


{- Orphan
instance HasParser x => Decode x where
    decode = decodeImpl
-}


wrap :: Parser String WrapRepr
wrap =
    foldMarkers
        [ marker $ "U" /\ Unit /\ (string "U" <#> const unit)
        -- , marker $ "V" /\ Value /\ empty
        ]


instance HasParser WrapRepr where
    parser = wrap


decodeImpl :: forall x. HasParser x => String -> Maybe x
decodeImpl s =
    case runParser s parser of
        Left _ -> Nothing
        Right result -> Just result