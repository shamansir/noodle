module Toolkit.Hydra2.Repr.Wrap.Parser where

import Data.FromToFile

import Prelude

import Parsing.Extra (marker, foldMarkers, parseBy)

import Control.Plus (empty)
import Control.Lazy (defer)

import Data.Foldable (foldr)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (fromFoldable, length) as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Parsing (Parser, runParser, fail)
import Parsing.String (string)
import Parsing.Combinators (try, many1, sepBy1, between, sepBy, replicateA)
import Parsing.String.Basic (space, number, intDecimal, alphaNum)
import Control.Alt ((<|>))


import Toolkit.Hydra2.Repr.Wrap
import Toolkit.Hydra2.Types as T
import Toolkit.Hydra2.Lang.SketchParser.Utils as U


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
        , marker $ "T" /\ Texture /\ (parser :: Parser String T.Texture)
        ]


value :: Parser String T.Value
value =
    foldMarkers
        [ marker $ "N" /\ T.Number /\ number
        , marker $ "0" /\ const T.None /\ string "V"
        , marker $ "U" /\ const T.Undefined /\ string "V"
        , marker $ "T" /\ const T.Time /\ string "V"
        , marker $ "MX" /\ const T.MouseX /\ string "V"
        , marker $ "MY" /\ const T.MouseY /\ string "V"
        , marker $ "W" /\ const T.Width /\ string "V"
        , marker $ "H" /\ const T.Height /\ string "V"
        , marker $ "PI" /\ const T.Pi /\ string "V"
        , marker $ "A" /\ T.Fft /\ (parser :: Parser String T.AudioBin)
        , marker $ "VA" /\ uncurry T.VArray /\ ((/\) <$> (defer \_ -> values) <*> (string " $$ " *> defer \_ -> ease))
        ]


audioBin :: Parser String T.AudioBin
audioBin = T.AudioBin <$> (string "@" *> intDecimal)


values :: Parser String T.Values
values =
    T.Values
        <$> Array.fromFoldable
        <$>
        (between (string "%% ") (string " %%")
            $ flip sepBy (string " <> ") $ defer $ \f -> value
        )


outputN :: Parser String T.OutputN
outputN =
    string "O" *> intDecimal <#> case _ of
        0 -> T.Output0
        1 -> T.Output1
        2 -> T.Output2
        3 -> T.Output3
        4 -> T.Output4
        _ -> T.Output0


sourceN :: Parser String T.SourceN
sourceN =
    string "S" *> intDecimal <#> const T.Source0


texture :: Parser String T.Texture
texture =
    foldMarkers
        [ marker $ "EMP" /\ const T.Empty /\ string "T"
        , marker $ "S" /\ T.Start /\ (parser :: Parser String T.Source)
        ]


source :: Parser String T.Source
source =
    foldMarkers
        [ marker $ "S" /\ T.Solid /\ parseArgs4 \r g b a -> { r, g, b, a }
        , marker $ "G" /\ T.Gradient /\ parseArgs1 \speed -> { speed }
        , marker $ "N" /\ T.Noise /\ parseArgs2 \scale offset -> { scale, offset }
        , marker $ "OSC" /\ T.Osc /\ parseArgs3 \frequency sync offset -> { frequency, sync, offset }
        , marker $ "SCP" /\ T.Shape /\ parseArgs3 \sides radius smoothing -> { sides, radius, smoothing }
        , marker $ "V" /\ T.Voronoi /\ parseArgs3 \scale speed blending -> { scale, speed, blending }
        , marker $ "O" /\ T.Load /\ (parser :: Parser String T.OutputN)
        , marker $ "X" /\ uncurry T.External /\ do
            src <- sourceN
            _ <- string T.argSep
            def <- extSource
            _ <- string T.argsEnd
            pure $ src /\ def

        ]


extSource :: Parser String T.ExtSource
extSource =
    foldMarkers
        [ marker $ "C" /\ T.Camera /\ intDecimal
        , marker $ "SK" /\ T.Sketch /\ (U.f1ts <$> many1 alphaNum)
        , marker $ "V" /\ const T.Video /\ string "X"
        , marker $ "U" /\ const T.Unclear /\ string "X"
        ]


ease :: Parser String T.Ease
ease = --pure T.Linear
    foldMarkers
        [ marker $ "LIN" /\ const T.Linear /\ string "E"
        , marker $ "FST" /\ T.Fast /\ defer \_ -> value
        , marker $ "SMT" /\ T.Smooth /\ defer \_ -> value
        , marker $ "OFF" /\ T.Offset /\ defer \_ -> value
        , marker $ "IOC" /\ const T.InOutCubic /\ string "E"
        , marker $ "FIT" /\ fit /\ do
                                low <- value
                                _ <- string " < "
                                high <- value
                                pure $ low /\ high

        ]
    where
        fit (low /\ high) = T.Fit { low, high }


instance HasParser WrapRepr where
    parser = wrap


instance HasParser T.Value where
    parser = value


instance HasParser T.AudioBin where
    parser = audioBin


instance HasParser T.Texture where
    parser = texture


instance HasParser T.Source where
    parser = source


instance HasParser T.OutputN where
    parser = outputN


decodeImpl :: forall x. HasParser x => String -> Maybe x
decodeImpl s =
    case runParser s parser of
        Left _ -> Nothing
        Right result -> Just result


parseArgs :: Int -> Parser String (Array T.Value)
parseArgs n =
    replicateA n (value <* string T.argSep)


parseArgsHelper :: forall x. Int → (Array T.Value → Maybe x) → Parser String x
parseArgsHelper n f =
    parseArgs n >>= \arr ->
        case f arr of
            Just res -> pure res
            Nothing -> fail $ "Required " <> show n <> " arguments but got " <> show (Array.length arr)


parseArgs1 :: forall x. (T.Value -> x) -> Parser String x
parseArgs1 f =
    parseArgsHelper 1 $ \arr -> f <$> arr !! 0


parseArgs2 :: forall x. (T.Value -> T.Value -> x) -> Parser String x
parseArgs2 f =
    parseArgsHelper 2 $ \arr -> f <$> arr !! 0 <*> arr !! 1


parseArgs3 :: forall x. (T.Value -> T.Value -> T.Value -> x) -> Parser String x
parseArgs3 f =
    parseArgsHelper 3 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2


parseArgs4 :: forall x. (T.Value -> T.Value -> T.Value -> T.Value -> x) -> Parser String x
parseArgs4 f =
    parseArgsHelper 4 $ \arr -> f <$> arr !! 0 <*> arr !! 1 <*> arr !! 2 <*> arr !! 3
