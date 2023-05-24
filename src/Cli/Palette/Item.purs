module Cli.Palette.Item where

import Prelude ((<$>), (<>), show)


import Color (Color)
import Color as Color

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


type Item =
    { color :: Maybe Color
    , repr :: String
    , label :: String
    , index :: Maybe (Int /\ Maybe Int)
    }


item :: Int -> String -> Int -> Int -> Int -> String -> Item
item idx hex r g b name =
    let color = Color.rgb r g b
    in { index : Just (idx /\ Nothing), color : Just color, repr : hex, label : name }


item' :: Int -> Int -> String -> Int -> Int -> Int -> String -> Item
item' idx idx' hex r g b name =
    let color = Color.rgb r g b
    in { index : Just (idx /\ Just idx'), color : Just color, repr : hex, label : name }


qitem :: String -> String -> Item
qitem color label =
    { index : Nothing, color : Nothing, repr : color, label }


qitem' :: String -> Item
qitem' color =
    qitem color color


rgb :: Int -> Int -> Int -> String -> Item
rgb r g b name =
    let color = Color.rgb r g b
    in { index : Nothing, color : Just color, repr : Color.toHexString color, label : name }


hsl :: Number -> Number -> Number -> String -> Item
hsl h s l name =
    let color = Color.hsl h s l
    in { index : Nothing, color : Just color, repr : Color.toHexString color, label : name }


rgbStr :: Item -> String
rgbStr item =
    case Color.toRGBA <$> item.color of
        Just { r, g, b } -> show r <> " " <> show g <> " " <> show b
        Nothing -> "? ? ?"


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


repr :: Item -> String
repr item =
    case Color.toHexString <$> item.color of
        Just hex -> "#" <> hex
        Nothing -> item.repr