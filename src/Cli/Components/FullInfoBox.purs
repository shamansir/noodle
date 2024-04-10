module Cli.Components.FullInfoBox where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Data.Text.Output.Blessed (render) as T

import Control.Monad.State (get) as State

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedOp (BlessedOp) as C

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys (fullInfoBox) as Key
import Cli.Style as Style
import Cli.State (State)
import Cli.Tagging as T

import Toolkit.Hydra.Repr.Info (class StatusLineInfo, statusLine)

import Toolkit.Hydra.Lang (formProgram) as Lang
import Toolkit.Hydra.Lang.ToCode (toCode, javaScript) as Lang
import Toolkit.Hydra.Repr.Wrap (WrapRepr) as Hydra

import Noodle.Id as Id


defaultText :: String
defaultText = "/* move mouse over something */"


-- FIXME: should have the ability to lock info of some specific node


component
    :: C.Blessed State
component =
    B.box Key.fullInfoBox
        [ Box.width $ Dimension.calc $ Coord.percents 40.0 <-> Coord.px 5
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 10
        , Box.top $ Offset.px 5
        , Box.left $ Offset.calc $ Coord.percents 60.0
        , Box.tags true
        , Box.content defaultText
        , Box.hidden true
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inputHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inputHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.commandLog
        , Style.patchBoxBorder
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                liftEffect $ Console.log $ show inputSelected
        -}
        ]
        [ ]


refresh :: forall state a. StatusLineInfo a => a -> C.BlessedOp state Effect
refresh with =
    Key.fullInfoBox >~ Box.setContent $ statusLine with


clear :: forall state. C.BlessedOp state Effect
clear =
    Key.fullInfoBox >~ Box.setContent defaultText


familyStatus :: forall state f m. IsSymbol f => Id.Family f -> C.BlessedOp state m
familyStatus family =
    Key.fullInfoBox >~ Box.setContent $ T.render $ T.familyDocs family


inputStatus :: forall state f i m. IsSymbol f => IsSymbol i => Id.Family' f -> Int -> Id.Input i -> Maybe Hydra.WrapRepr -> C.BlessedOp state m
inputStatus family idx inputId maybeRepr =
    Key.fullInfoBox >~ Box.setContent $ T.render $ T.inputStatusLine family idx inputId maybeRepr


outputStatus :: forall state f o m. IsSymbol f => IsSymbol o => Id.Family' f -> Int -> Id.Output o -> Maybe Hydra.WrapRepr -> C.BlessedOp state m
outputStatus family idx outputId maybeRepr =
    Key.fullInfoBox >~ Box.setContent $ T.render $ T.outputStatusLine family idx outputId maybeRepr


removeStatus :: forall state f m. IsSymbol f ⇒ Id.Family f -> C.BlessedOp state m
removeStatus family =
    Key.fullInfoBox >~ Box.setContent $ T.render $ T.removeStatusLine family