module Halogen.HTML.Properties.Extra where

import Prelude

import Halogen.HTML.Properties as HHP


data Position
    = Rel
    | Abs


-- positionAt :: Position -> { x :: Number, y :: Number } ->
position :: forall r i. Position → { x ∷ Number , y ∷ Number } → HHP.IProp ( style ∷ String | r ) i
position pos { x, y } =
    HHP.style $ "position: " <> posString <> "; left: " <> show x <> "px; top: " <> show y <> "px;"
    where
        posString = case pos of
            Abs -> "absolute"
            Rel -> "relative"