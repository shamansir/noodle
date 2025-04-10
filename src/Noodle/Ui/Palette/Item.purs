module Noodle.Ui.Palette.Item where

import Prelude

import Data.Maybe (maybe)
import Data.Either (Either(..), either)

import Color (Color)
import Color as Color

import Halogen.Svg.Attributes.Color as HC

import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


-- TODO: We intentionally store all the modes here, for now,
-- but probably we should just ensure we can convert forth and back b/w formats in reliable way w/o any loss

newtype ColorId = ColorId String


newtype Hex = Hex String


data Index
    = Single Int
    | Level Int Int
    | SingleS String
    | LevelS String Int


type Item =
    { color :: Either ColorId Color
    , label :: String
    , index :: Maybe Index
    }


fromId :: ColorId -> Item
fromId (ColorId colorId) =
    { color : Left $ ColorId colorId
    , label : colorId
    , index : Nothing
    }


reprAndName :: ColorId -> String -> Item
reprAndName colorId label =
    { color : Left colorId
    , label
    , index : Nothing
    }


rgb :: Int -> Int -> Int -> String -> Item
rgb r g b name =
    { color : Right $ Color.rgb r g b
    , label : name
    , index : Nothing
    }


hsl :: Number -> Number -> Number -> String -> Item
hsl h s l name =
    { color : Right $ Color.hsl h s l
    , label : name
    , index : Nothing
    }


hex :: Hex -> String -> Item
hex (Hex hexStr) name =
    { color : maybe (Left $ ColorId hexStr) Right $ Color.fromHexString hexStr
    , label : name
    , index : Nothing
    }


rgbStr :: Item -> String
rgbStr item =
    case Color.toRGBA <$> item.color of
        Right { r, g, b } -> show r <> " " <> show g <> " " <> show b
        Left _ -> "? ? ?"


idxHexRgb :: Int -> String -> Int -> Int -> Int -> String -> Item
idxHexRgb idx hex r g b name =
    { index : Just $ Single idx
    , color : Right $ Color.rgb r g b
    , label : name
    }


idx2HexRgb :: Int -> Int -> String -> Int -> Int -> Int -> String -> Item
idx2HexRgb idx idx2 hex r g b name =
    { index : Just $ Level idx idx2
    , color : Right $ Color.rgb r g b
    , label : name
    }


hexRgbHsl :: String -> Int -> Int -> Int -> Int -> Int -> Int -> String -> Item
hexRgbHsl hex r g b h s l name =
    { index : Nothing
    , color : Right $ Color.rgb r g b
    , label : name
    }


strRgb :: String -> Int -> Int -> Int -> Item
strRgb idx r g b =
    { index : Just $ SingleS idx
    , color : Right $ Color.rgb r g b
    , label : idx
    }


str2Rgb :: String -> Int -> Int -> Int -> Int -> Item
str2Rgb idxl idx r g b =
    { index : Just $ LevelS idxl idx
    , color : Right $ Color.rgb r g b
    , label : idxl <> "-" <> show idx
    }


strHex :: String -> Hex -> Item
strHex idx hexv =
    (hex hexv idx) { index = Just $ SingleS idx }


str2Hex :: String -> Int -> Hex -> Item
str2Hex idxl idx hexv =
    (hex hexv $ idxl <> "-" <> show idx) { index = Just $ LevelS idxl idx }


fullInfo :: Item -> String
fullInfo item =
    item.label <> "\t" <> colorValueStr <> indexString
    where
        indexString = case item.index of
            Just (Single n) -> "\t" <> show n
            Just (Level n1 n2) -> "\t" <> show n1 <> " (" <> show n2 <> ")"
            Just (SingleS sl) -> "\t" <> sl
            Just (LevelS sl1 sl2) -> "\t" <> sl1 <> show sl2
            Nothing -> ""
        colorValueStr =
            case item.color of
                Right color ->
                    let
                        { r, g, b, a } = Color.toRGBA color
                        hexStr = Color.toHexString color
                        { h, s, l } = Color.toHSLA color
                    in
                        "/" <> show r <> " " <> show g <> " " <> show b <> " / " <> hexStr <> " / " <> show h <> " " <> show s <> " " <> show l
                Left (ColorId colorId) -> colorId


reprOf :: Item -> String
reprOf item =
    case Color.toHexString <$> item.color of
        Right hexStr -> hexStr
        Left (ColorId str) -> str


colorOf :: Item -> Color
colorOf item =
    case item.color of
        Right color -> color
        Left _ -> Color.black -- FIXME: have a table for colorIds as in CSS, for example


hColorOf :: Item -> HC.Color
hColorOf = colorOf >>> convert
    where
        convert c =
            case Color.toRGBA c of
                { r, g, b, a } -> HC.RGBA r g b a


transparent :: Item
transparent =
    { color : Right $ Color.rgba 0 0 0 0.0
    , label : "transparent"
    , index : Nothing
    }