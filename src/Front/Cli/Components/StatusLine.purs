module Cli.Components.StatusLine where

import Prelude

import Type.Proxy (Proxy)

import Effect (Effect)
import Data.Maybe (Maybe)
import Data.Text.Output.Blessed (singleLine) as T

import Blessed ((>~))
import Blessed as B

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedOp (BlessedOp) as C

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Cli.Keys (statusLine) as Key
import Cli.Style as Style
import Cli.State (State)

import Noodle.Id as Id
-- import Noodle.Family.Def as Family
import Noodle.Toolkit (class IsToolkit, class MarkToolkit, class HasRepr)
import Noodle.Fn.ToFn (class PossiblyToFn)
import Noodle.Ui.Cli.Tagging (familyStatusLine, inletStatusLine, outletStatusLine, removeStatusLine)  as T
import Noodle.Ui.Cli.Tagging.At (class At) as Tagged
import Noodle.Ui.Cli.Tagging.At (StatusLine) as At


{-}
width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InputButton.widthN + 1) * count -}


component
    :: forall state. C.Blessed state
component =
    B.box Key.statusLine
        [ Box.width $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 2
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 2
        , Box.left $ Offset.px 1
        , Box.tags true
        , Box.content ""
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inputHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inputHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.statusLine
        {- , Core.on ListBar.Select
            \_ _ -> do
                CC.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                CC.log $ show inputSelected
        -}
        ]
        [ ]


familyStatus
    :: forall tk pstate fs repr m
     . MarkToolkit tk
    => HasRepr tk repr
    => Tagged.At At.StatusLine repr
    => PossiblyToFn tk (Maybe chrepr) (Maybe chrepr) Id.FamilyR
    => Proxy tk
    -> Id.FamilyR
    -> C.BlessedOp (State tk pstate fs repr m) Effect
familyStatus ptk familyR =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.familyStatusLine ptk familyR


inletStatus :: forall tk pstate fs repr m. Tagged.At At.StatusLine repr => Id.FamilyR -> Int -> Id.InletR -> Maybe repr -> C.BlessedOp (State tk pstate fs repr m) Effect
inletStatus family idx inputId maybeRepr =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.inletStatusLine family idx inputId maybeRepr


outletStatus :: forall tk pstate fs repr m. Tagged.At At.StatusLine repr => Id.FamilyR -> Int -> Id.OutletR -> Maybe repr -> C.BlessedOp (State tk pstate fs repr m) Effect
outletStatus family idx outputId maybeRepr =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.outletStatusLine family idx outputId maybeRepr


removeStatus :: forall tk pstate fs repr m. Id.FamilyR -> C.BlessedOp (State tk pstate fs repr m) Effect
removeStatus family =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.removeStatusLine family


clear :: forall state m. C.BlessedOp state m
clear =
    Key.statusLine >~ Box.setContent ""