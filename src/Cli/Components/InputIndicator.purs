module Cli.Components.InputIndicator where

import Prelude


import Effect (Effect)

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Boxes.Box.Option (content, height, left, top, width, tags) as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.UI.Base.Element.PropertySet (setLeft, setTop) as Element
import Blessed.UI.Base.Element.Method (hide, show, setFront) as Element

import Blessed.Tagger (render) as T

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style

import Cli.Tagging (inputHover) as T



data Status
    = Hover
    | Off
    -- TODO: Editor


contentFor :: Status -> String
contentFor Off = ""
contentFor Hover = T.render $ T.inputHover


component ∷ Core.Blessed State
component =
    B.buttonAnd Key.inputIndicator
        [ Box.content $ contentFor Off
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Style.indicator
        ]
        []
        \_ -> do
            Key.inputIndicator >~ Element.hide



-- TODO: move to the corresponding input on hover

move :: forall state. { x :: Int, y :: Int } -> BlessedOp state Effect
move { x, y } = do
    Key.inputIndicator >~ Element.setLeft $ Offset.px x
    Key.inputIndicator >~ Element.setTop $ Offset.px y


updateStatus :: Status -> BlessedOp State Effect
updateStatus status = do
    Key.inputIndicator >~ Element.setFront
    Key.inputIndicator >~ Box.setContent $ contentFor status
    Key.inputIndicator >~ Element.show


hide :: forall state. BlessedOp state Effect
hide =
    Key.inputIndicator >~ Element.hide