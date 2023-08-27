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
import Parsing.String.Basic (space, number, intDecimal)
import Control.Alt ((<|>))


import Toolkit.Hydra2.Repr.Wrap
import Toolkit.Hydra2.Types as T


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
        , marker $ "V" /\ Value /\ (parser :: Parser String T.Value)
        ]


value :: Parser String T.Value
value =
    foldMarkers
        [ marker $ "N" /\ T.Number /\ number
        , marker $ "0" /\ const T.None /\ string "X"
        , marker $ "U" /\ const T.Undefined /\ string "U"
        , marker $ "T" /\ const T.Time /\ string "T"
        , marker $ "X" /\ const T.MouseX /\ string "MX"
        , marker $ "Y" /\ const T.MouseY /\ string "MY"
        , marker $ "W" /\ const T.Width /\ string "W"
        , marker $ "H" /\ const T.Height /\ string "H"
        , marker $ "P" /\ const T.Pi /\ string "PI"
        , marker $ "A" /\ T.Fft /\ (parser :: Parser String T.AudioBin)
        ]


audioBin :: Parser String T.AudioBin
audioBin = T.AudioBin <$> (string "@" *> intDecimal)


-- values :: Parser String T.Values
-- values =


instance HasParser WrapRepr where
    parser = wrap


instance HasParser T.Value where
    parser = value


instance HasParser T.AudioBin where
    parser = audioBin


decodeImpl :: forall x. HasParser x => String -> Maybe x
decodeImpl s =
    case runParser s parser of
        Left _ -> Nothing
        Right result -> Just result