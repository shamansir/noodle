module Cli.Components.NodeBox.OutletsBox where

import Prelude



import Control.Monad.State as State

import Effect (Effect)
import Type.Proxy (Proxy)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))

import Blessed as B

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Key (Key) as C
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedSubj (ListBar)

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option (keys, mouse) as List
import Blessed.UI.Lists.ListBar.Option (commands) as ListBar
import Blessed.Internal.Core as Core

import Cli.Keys (NodeBoxKey, OutletsBoxKey)
import Cli.Style as Style
import Cli.State (State)

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 as Patch

import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


component
    -- forall id r f state fs iis rli is rlo os repr_is repr_os
    -- . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    -- => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    -- => FromToReprRow rli is Hydra.WrapRepr
    -- => FromToReprRow rlo os Hydra.WrapRepr
    -- => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -- => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    :: Patch.HoldsNode Effect
    -> NodeBoxKey
    -> OutletsBoxKey
    -> Array (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    -> C.Blessed State
component nodeHolder nextNodeBox nextOutletsBox os =
    B.box nextOutletsBox
        [ Box.width $ Dimension.percents 90.0
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.px 2
        , Box.left $ Offset.px 0
        -- , ListBar.commands $ mapWithIndex (\idx hoinr -> Node.withOutputInNodeMRepr hoinr (outletHandler nodeHolder nextNodeBox idx)) os
        -- , List.mouse true
        -- , List.keys true
        , Style.inletsOutlets
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "outlet"
                outletSelected <- List.selected ~< nextOutletsBox
                liftEffect $ Console.log $ show outletSelected
        -}
        ]
        [
        ]


outletHandler
    :: forall f nstate o dout is os os'
     . IsSymbol f
    => Id.HasOutput o dout os' os
    => ToRepr dout Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr dout
    => Patch.HoldsNode Effect
    -> NodeBoxKey
    -> Int
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-outlets-bar" State
outletHandler nodeHolder nextNodeBox index pdout node output =
    Id.reflect output /\ [] /\ \_ _ -> do
        -- liftEffect $ Console.log $ "handler " <> oname
        State.modify_
            (_
                { lastClickedOutlet =
                    Just
                        { index, subj : Id.reflect output, nodeKey : nextNodeBox, nodeId : Id.holdNodeId (Node.id node), outputId : Node.holdOutputInNodeMRepr pdout node output, node : nodeHolder } })
