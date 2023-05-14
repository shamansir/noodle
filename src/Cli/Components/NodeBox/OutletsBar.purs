module Cli.Components.NodeBox.OutletsBar where

import Prelude



import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (Repr, class FromRepr, class ToRepr, class FromToReprRow, toRepr, fromRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Type.Row (type (+))

import Blessed ((>~), (~<))
import Blessed (exit) as Blessed
import Blessed as B

import Blessed.Core.Border as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Key (Key) as C
import Blessed.Core.Key as Key
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Orientation as Orientation

import Blessed.Internal.Core (Blessed, Node, NodeAnd, run, runAnd) as C
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp, BlessedOpM)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)

import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Element.Property as Element
import Blessed.UI.Base.Element.PropertySet as Element
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event as List
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.List.Property as List
import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Lists.ListBar.Option as ListBar
import Blessed.UI.Lists.ListBar.Method as ListBar
import Blessed.Internal.Core as Core

import Cli.Keys (NodeBoxKey, OutletsBarKey)
import Cli.Keys as Key
import Cli.Style as Style
import Cli.State (State, Link(..), OutletIndex(..), InletIndex(..))
import Cli.State.NwWraper (wrapN, unwrapN)
import Cli.Components.Link as Link

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Network2 as Network
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Repr (nodeToRepr, nodeToMapRepr, Repr(..), class HasRepr, class ToReprHelper) as R

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.BlessedRepr as Hydra


outletHandler :: forall f nstate o dout is os os'. IsSymbol f => Id.HasOutput o dout os' os => ToRepr dout Hydra.BlessedRepr => FromRepr Hydra.BlessedRepr dout => Patch.HoldsNode Effect -> NodeBoxKey -> Int -> Proxy dout -> Noodle.Node f nstate is os Effect -> Id.Output o -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-outlets-bar" State
outletHandler nodeHolder nextNodeBox index pdout node output =
    Id.reflect output /\ [] /\ \_ _ -> do
        -- liftEffect $ Console.log $ "handler " <> oname
        State.modify_
            (_
                { lastClickedOutlet =
                    Just
                        { index, subj : Id.reflect output, nodeKey : nextNodeBox, nodeId : Id.holdNodeId (Node.id node), outputId : Node.holdOutputInNodeMRepr pdout node output, node : nodeHolder } })
        -- onOutletSelect nodeId output nextNodeBox idx (Id.reflect output)


component
    :: forall id r f state fs iis rli is rlo os repr_is repr_os
     . {- Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.BlessedRepr state
    => FromToReprRow rli is Hydra.BlessedRepr
    => FromToReprRow rlo os Hydra.BlessedRepr
    => Node.TestNodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.BlessedRepr)
    => Node.TestNodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)
    => -} Patch.HoldsNode Effect
    -> NodeBoxKey
    -> OutletsBarKey
    -- -> Id.Family f
    -- -> Family.Def state is os Effect
    -> Array (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)
    -> _
component nodeHolder nextNodeBox nextOutletsBar os =
    B.listbar nextOutletsBar
        [ Box.width $ Dimension.percents 90.0
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.px 2
        , Box.left $ Offset.px 0
        -- , List.items os
        , ListBar.commands $ mapWithIndex (\idx hoinr -> Node.withOutputInNodeMRepr hoinr (outletHandler nodeHolder nextNodeBox idx)) os
        -- , ListBar.commands $ mapWithIndex outletHandler $ Id.reflect' <$> os
        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex outletHandler $ os
        , List.mouse true
        , List.keys true
        , Style.inletsOutlets
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "outlet"
                outletSelected <- List.selected ~< nextOutletsBar
                liftEffect $ Console.log $ show outletSelected
        -}
        ]
        [
        ]
