module Halogen.Svg.Attributes.Color.Extra where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Svg.Attributes.Color


setAlpha :: Number -> Color -> Maybe Color
setAlpha = adjustAlpha <<< const


adjustAlpha :: (Maybe Number -> Number) -> Color -> Maybe Color
adjustAlpha f = case _ of
    Named color -> Nothing
    RGB r g b -> Just $ RGBA r g b $ f Nothing
    RGBA r g b a -> Just $ RGBA r g b $ f $ Just a