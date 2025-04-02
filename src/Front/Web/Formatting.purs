module Web.Formatting where

import Prelude

import Data.Array (intersperse) as Array

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.Extra as HSX

import Data.Text.Format (Tag(..))


renderFormatting :: forall p i. Tag -> HH.HTML p i
renderFormatting = case _ of
    Plain str -> HS.text [] [ HH.text str ]
    Empty -> HSX.none
    Format _ tag -> renderFormatting tag
    Align _ tag -> renderFormatting tag
    Pair tagA tagB -> HS.g [] [ renderFormatting tagA, renderFormatting tagB ]
    Join tag tags -> HS.g [] $ renderFormatting <$> Array.intersperse tag tags
    Split tagA tagB -> HS.g [] [ renderFormatting tagA, renderFormatting tagB ] -- FIXME: what it does?
    Wrap withL tag withR -> HS.g [] [ renderFormatting withL, renderFormatting tag, renderFormatting withR ] -- FIXME
    _ -> HSX.none
    -- Plain str -> HS.text [] [ HH.text str ]
    -- FIXME: render to HTML instead, try inline raw HTML code from String