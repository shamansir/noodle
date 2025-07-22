module Web.Formatting where

import Prelude

import Data.Array (intersperse) as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Color as C

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.Extra as HSX

import Data.Text.Format (Tag(..), Format(..))
import Web.Layer (TargetLayer(..))


renderFormatting :: forall p i. TargetLayer -> Tag -> HH.HTML p i
renderFormatting SVG = case _ of -- FIXME: use TSpan?
    Plain str -> HS.text [] [ HH.text str ]
    Empty -> HSX.none
    Format (Fg fgColor) tag ->
        case fgColor of
            Left strColor ->
                HS.g [ HSA.fill $ Just $ HC.Named strColor ] [ renderFormatting SVG tag ]
            Right vColor ->
                case C.toRGBA vColor of
                    { r, g, b } -> HS.g [ HSA.fill $ Just $ HC.RGB r g b ] [ renderFormatting SVG tag ]
    Format (Bg bgColor) tag ->
        case bgColor of
            Left strColor -> renderFormatting SVG tag
            Right vColor -> renderFormatting SVG tag
    Format _ tag -> renderFormatting SVG tag
    Align _ tag -> renderFormatting SVG tag
    Pair tagA tagB -> HS.g [] [ renderFormatting SVG tagA, renderFormatting SVG tagB ]
    Join tag tags -> HS.g [] $ renderFormatting SVG <$> Array.intersperse tag tags
    Split tagA tagB -> HS.g [] [ renderFormatting SVG tagA, renderFormatting SVG tagB ] -- FIXME: what it does?
    Wrap withL tag withR -> HS.g [] [ renderFormatting SVG withL, renderFormatting SVG tag, renderFormatting SVG withR ] -- FIXME
    _ -> HSX.none
    -- Plain str -> HS.text [] [ HH.text str ]
    -- FIXME: render to HTML instead, try inline raw HTML code from String


renderFormatting HTML = case _ of
    Plain str -> HH.span [] [ HH.text str ]
    Empty -> HH.text ""
    Format (Fg fgColor) tag ->
        case fgColor of
            Left strColor ->
                HH.span [ HHP.style $ "color: " <> HC.printColor (Just $ HC.Named strColor) ] [ renderFormatting HTML tag ]
            Right vColor ->
                case C.toRGBA vColor of
                    { r, g, b } -> HH.span [ HHP.style $ "color: " <> HC.printColor (Just $ HC.RGB r g b) ] [ renderFormatting HTML tag ]
    Format (Bg bgColor) tag ->
        case bgColor of
            Left strColor -> HH.span [ HHP.style $ "background-color: " <> HC.printColor (Just $ HC.Named strColor) ] [ renderFormatting HTML tag ]
            Right vColor ->
                case C.toRGBA vColor of
                    { r, g, b } -> HH.span [ HHP.style $ "background-color: " <> HC.printColor (Just $ HC.RGB r g b) ] [ renderFormatting HTML tag ]
    Format _ tag -> renderFormatting HTML tag
    Align _ tag -> renderFormatting HTML tag
    Pair tagA tagB -> HH.span [] [ renderFormatting HTML tagA, renderFormatting HTML tagB ]
    Join tag tags -> HH.span [] $ renderFormatting HTML <$> Array.intersperse tag tags
    Split tagA tagB -> HH.span [] [ renderFormatting HTML tagA, renderFormatting HTML tagB ] -- FIXME: what it does?
    Wrap withL tag withR -> HH.span [] [ renderFormatting HTML withL, renderFormatting HTML tag, renderFormatting HTML withR ] -- FIXME
    _ -> HSX.none
