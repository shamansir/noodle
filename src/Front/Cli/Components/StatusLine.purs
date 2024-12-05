module Cli.Components.StatusLine where

import Prelude

import Type.Proxy (Proxy)

import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe)
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (class IsSymbol)
import Data.Text.Format as T
import Data.Text.Output.Blessed (singleLine) as T

import Blessed ((>~))
import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedOp (BlessedOp) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box

import Cli.Keys (statusLine) as Key
import Cli.Style as Style
import Cli.State (State)
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Tagging.At as T

-- import Cli.Components.NodeBox.InputButton as InputButton

import Noodle.Id as Id
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
-- import Noodle.Family.Def as Family
import Noodle.Toolkit (class IsToolkit, class MarkToolkit)
import Noodle.Fn.ToFn (class PossiblyToFn, FnS, possiblyToFn)


import Noodle.Ui.Cli.Palette as Palette
import Noodle.Ui.Cli.Palette.Item (crepr) as C
import Noodle.Ui.Cli.Palette.Item (Item, fullInfo) as Palette
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico
import Noodle.Ui.Cli.Tagging.At (class At) as Tagged
import Noodle.Ui.Cli.Tagging.At (StatusLine, ChannelLabel, Documentation, InfoNode, statusLine, channelLabel, documentation, infoNode) as At


{-}
width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InputButton.widthN + 1) * count -}


component
    :: forall tk pstate fs repr m. C.Blessed (State tk pstate fs repr m)
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
                liftEffect $ Console.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                liftEffect $ Console.log $ show inputSelected
        -}
        ]
        [ ]


familyStatus
    :: forall tk pstate fs repr m
     . IsToolkit tk
    => Tagged.At At.Documentation repr
    => MarkToolkit tk repr
    => PossiblyToFn tk repr repr Id.FamilyR
    => Proxy tk
    -> Id.FamilyR
    -> C.BlessedOp (State tk pstate fs repr m) Effect
familyStatus ptk familyR =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.familyShortInfo ptk familyR


inletStatus :: forall tk pstate fs repr m. T.At T.StatusLine repr => Id.FamilyR -> Int -> Id.InletR -> Maybe repr -> C.BlessedOp (State tk pstate fs repr m) Effect
inletStatus family idx inputId maybeRepr =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.inletStatusLine family idx inputId maybeRepr


outletStatus :: forall tk pstate fs repr m. T.At T.StatusLine repr => Id.FamilyR -> Int -> Id.OutletR -> Maybe repr -> C.BlessedOp (State tk pstate fs repr m) Effect
outletStatus family idx outputId maybeRepr =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.outletStatusLine family idx outputId maybeRepr


removeStatus :: forall tk pstate fs repr m. Id.FamilyR -> C.BlessedOp (State tk pstate fs repr m) Effect
removeStatus family =
    Key.statusLine >~ Box.setContent $ T.singleLine $ T.removeStatusLine family


clear :: forall state m. C.BlessedOp state m
clear =
    Key.statusLine >~ Box.setContent ""