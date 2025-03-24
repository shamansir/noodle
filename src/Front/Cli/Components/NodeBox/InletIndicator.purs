module Cli.Components.NodeBox.InletIndicator where

import Prelude


import Prelude


import Effect (Effect)
import Effect.Exception (Error)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Text.Output.Blessed (singleLine) as T

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

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style


import Noodle.Fn.Shape.Temperament (Temperament)

import Noodle.Ui.Tagging (inletHover) as T




data Status
    = Hover Temperament
    | Off
    -- TODO: Editor


contentFor :: Status -> String
contentFor Off = ""
contentFor (Hover temp) = T.singleLine $ T.inletHover temp


component ∷ forall state. Core.Blessed state
component =
    B.buttonAnd Key.inletIndicator
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
            Key.inletIndicator >~ Element.hide



-- TODO: move to the corresponding input on hover

move :: forall state m. MonadThrow Error m => { x :: Int, y :: Int } -> BlessedOp state m
move { x, y } = do
    Key.inletIndicator >~ Element.setLeft $ Offset.px x
    Key.inletIndicator >~ Element.setTop  $ Offset.px y


updateStatus :: forall state m. Status -> BlessedOp state m
updateStatus status = do
    Key.inletIndicator >~ Element.setFront
    Key.inletIndicator >~ Box.setContent $ contentFor status
    Key.inletIndicator >~ Element.show


hide :: forall state m. BlessedOp state m
hide =
    Key.inletIndicator >~ Element.hide