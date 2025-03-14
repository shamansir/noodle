module Cli.Components.NodeBox.RemoveButton where

import Prelude


import Effect (Effect)

import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (for_)
import Data.Map (lookup, empty) as Map
import Data.Text.Output.Blessed (singleLine) as T

import Control.Monad.State (get, modify_) as State

import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift', runOnUnit) as Blessed

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (PatchBoxKey, NodeBoxKey, RemoveButtonKey, InfoBoxKey)
import Cli.Keys (mainScreen, statusLine, patchBox) as Key
import Cli.Style as Style
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Cli.State (State)
import Cli.State (currentPatch, replacePatch, LastKeys) as CState
import Cli.Components.Link as CLink
import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.SidePanel as SidePanel
import Cli.Components.StatusLine as SL
import Cli.Components.SidePanel.Documentation as Doc
import Cli.Components.SidePanel.CommandLog as CL
import Cli.Components.SidePanel.Tree as TP
import Cli.Components.NodeBox.InletIndicator as II
import Cli.Components.NodeBox.OutletIndicator as OI

import Noodle.Id as Id
import Noodle.Patch as Patch
import Noodle.Network as Network
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id) as RawNode
import Noodle.Wiring (class Wiring)
import Noodle.Text.NdfFile.Command.Quick as QOp
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
                        nextCurrentPatch <- Blessed.lift' $ Patch.disconnectAllFromTo (RawNode.id node) currentPatch
                        let nextLinks /\ linksToRemove = CLink.forgetAllFromTo (RawNode.id node) state.links
                        State.modify_ (\s ->
                            s
                                # CState.replacePatch (Patch.id currentPatch) nextCurrentPatch
                                # _ { links = nextLinks }
                        )
                        Blessed.runOnUnit $ CLink.removeAllOf Key.patchBox linksToRemove
                        keys.nodeBox >~ Node.detach
                        State.modify_
                            $ CState.replacePatch (Patch.id currentPatch)
                            $ Patch.removeNode (RawNode.id node) nextCurrentPatch
                        CL.trackCommand $ QOp.removeNode (RawNode.id node)
                        SidePanel.refresh TP.sidePanel
                        Key.mainScreen >~ Screen.render
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