module Halogen.HTML.Properties.Extra where

import Prelude

import Halogen.HTML.Properties as HHP


data Position
    = Rel
    | Abs


position :: forall r i. Position -> { x :: Number , y :: Number } -> HHP.IProp ( style :: String | r ) i
position pos =
    HHP.style <<< position_ pos


position_ :: Position -> { x :: Number, y :: Number } -> String
position_ pos { x, y } =
    "position: " <> posString <> "; left: " <> show x <> "px; top: " <> show y <> "px;"
    where
        posString = case pos of
            Abs -> "absolute"
            Rel -> "relative"


fontSize :: forall r i. Number -> HHP.IProp ( style :: String | r ) i
fontSize =
    HHP.style <<< fontSize_


fontSize_ :: Number -> String
fontSize_ n = "font-size: " <> show n <> "px;"