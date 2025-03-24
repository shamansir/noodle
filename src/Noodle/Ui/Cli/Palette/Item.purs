module Noodle.Ui.Cli.Palette.Item where

import Prelude ((<$>), (<>), show)


import Color (Color)
import Color as Color

import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


-- TODO: We intentionally store all the modes here, for now,
-- but probably we should just ensure we can convert forth and back b/w formats in reliable way w/o any loss
type Item =
    { color :: Maybe Color
    , repr :: String
    , src ::
        { hex :: Maybe String
        , rgb :: Maybe { r :: Int, g :: Int, b :: Int }
        , hsl :: Maybe { h :: Number, s :: Number, l :: Number }
        }
    , label :: String
    , index :: Maybe (Int /\ Maybe Int)
    }


justName :: String -> Item
justName color =
    reprName color color


reprName :: String -> String -> Item
reprName color label =
    { index : Nothing
    , color : Nothing
    , repr : color
    , src :
        { hex : Nothing
        , rgb : Nothing
        , hsl : Nothing
        }
    , label
    }


rgb :: Int -> Int -> Int -> String -> Item
rgb r g b name =
    let color = Color.rgb r g b
    in
        { index : Nothing
        , repr : Color.toHexString color
        , color : Just color
        , src :
            { hex : Nothing
            , rgb : Just { r, g, b }
            , hsl : Nothing
            }
        , label : name
        }


hsl :: Number -> Number -> Number -> String -> Item
hsl h s l name =
    let color = Color.hsl h s l
    in
        { index : Nothing
        , repr : Color.toHexString color
        , color : Just color
        , src :
            { hex : Nothing
            , rgb : Nothing
            , hsl : Just { h, s, l }
            }
        , label : name
        }


rgbStr :: Item -> String
rgbStr item =
    case Color.toRGBA <$> item.color of
        Just { r, g, b } -> show r <> " " <> show g <> " " <> show b
        Nothing -> "? ? ?"


idxHexRgb :: Int -> String -> Int -> Int -> Int -> String -> Item
idxHexRgb idx hex r g b name =
    let color = Color.rgb r g b
    in
        { index : Just (idx /\ Nothing)
        , color : Just color
        , repr : hex
        , src :
            { hex : Just hex
            , rgb : Just { r, g, b }
            , hsl : Nothing
            }
        , label : name
        }


idx2HexRgb :: Int -> Int -> String -> Int -> Int -> Int -> String -> Item
idx2HexRgb idx idx2 hex r g b name =
    let color = Color.rgb r g b
    in
        { index : Just (idx /\ Just idx2)
        , color : Just color
        , repr : hex
        , src :
            { hex : Just hex
            , rgb : Just { r, g, b }
            , hsl : Nothing
            }
        , label : name
        }


hexRgbHsl :: String -> Int -> Int -> Int -> Int -> Int -> Int -> String -> Item
hexRgbHsl hex r g b h s l name =
    let color = Color.rgb r g b
    in
        { index : Nothing
        , color : Just color
        , repr : hex
        , src :
            { hex : Just hex
            , rgb : Just { r, g, b }
            , hsl : Just
                { h : Int.toNumber h -- FIXME: Convert units
                , s : Int.toNumber s -- FIXME: Convert units
                , l : Int.toNumber l -- FIXME: Convert units
                }
            }
        , label : name
        }



fullInfo :: Item -> String
fullInfo item =
    item.label <> "\t" <> item.repr <> " " <> colorValueStr
    where
        colorValueStr =
            case (Color.toRGBA <$> item.color) /\ (Color.toHexString <$> item.color) of
                Just { r, g, b } /\ Just hex -> "/" <> show r <> " " <> show g <> " " <> show b <> "/ " <> hex
                Just { r, g, b } /\ Nothing -> "/" <> show r <> " " <> show g <> " " <> show b <> "/"
                Nothing /\ Just hex -> hex
                Nothing /\ Nothing -> ""


reprOf :: Item -> String
reprOf item =
    case Color.toHexString <$> item.color of
        Just hex -> hex
        Nothing -> item.repr


colorOf :: Item -> Color
colorOf item =
    case item.color of
        Just color -> color
        Nothing -> case item.src.rgb of
            Just { r, g, b } ->
                Color.rgb r g b
            Nothing -> Color.black
