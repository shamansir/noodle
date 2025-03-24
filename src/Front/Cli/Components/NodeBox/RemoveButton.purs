module Cli.Components.NodeBox.RemoveButton where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Text.Output.Blessed (singleLine) as T

import Control.Monad.State (get) as State

import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))

import Blessed.UI.Boxes.Box.Option (content, height, left, tags, top, width) as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.Internal.Core as Core
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (mainScreen) as Key
import Cli.Style as Style
import Cli.State (State)
import Cli.State (currentPatch, LastKeys) as CState
import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.NodeBox.InletIndicator as II
import Cli.Components.NodeBox.OutletIndicator as OI
import Front.Cli.Actions as Actions

import Noodle.Id as Id
import Noodle.Patch as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id) as RawNode
import Noodle.Wiring (class Wiring)

import Noodle.Ui.Tagging (removeButtonOut, removeButtonOver) as T

-- import Noodle.Node (unsafeDisconnect) as Node
-- import Noodle.Patch (removeNode, allLinksOf, withLink) as Patch


component
    :: forall tk fs fstate pstate strepr chrepr m
     . Wiring m
    => Offset
    -> Id.FamilyR
    -> Raw.Node fstate chrepr m
    -> CState.LastKeys
    -> Core.Blessed (State _ tk pstate fs strepr chrepr m)
component topOffset family node keys =
    B.button keys.removeButton
        [ Box.content $ T.singleLine $ T.removeButtonOut
        , Box.top topOffset
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 3
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width  $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            \_ _ -> do
                OI.hide
                II.hide
                state <- State.get
                let mbCurrentPatch = CState.currentPatch state
                case mbCurrentPatch of
                    Just currentPatch -> do
                        Actions.removeNode Actions.Track (Patch.id currentPatch) (RawNode.id node) keys.nodeBox
                    Nothing -> pure unit
        , Core.on Element.MouseOver
            \_ _ -> do
                keys.removeButton >~ Box.setContent $ T.singleLine $ T.removeButtonOver
                keys.infoBox >~ IB.removeInfo
                SL.removeStatus family
                -- REM -- FI.removeStatus family
                Key.mainScreen >~ Screen.render -- FIXME: refresh only the area
        , Core.on Element.MouseOut
            \_ _ -> do
                keys.removeButton >~ Box.setContent $ T.singleLine $ T.removeButtonOut
                -- Info box : delete this node
                keys.infoBox >~ IB.clear
                SL.clear
                -- REM -- FI.clear
                Key.mainScreen >~ Screen.render -- FIXME: refresh only the area
        ]
        []