module Cli.Components.NodeBox.InletsBox where

import Prelude

import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Array ((:))
import Data.Array (length) as Array
import Data.Map (Map)
import Data.Map as Map

import Blessed ((>~))
import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Key (Key) as C
import Blessed.Internal.NodeKey (next) as Key
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedSubj (Line, ListBar)

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option (keys, mouse) as List
import Blessed.UI.Lists.ListBar.Option (autoCommandKeys, commands) as ListBar
import Blessed.Internal.Core as Core

import Cli.Keys (NodeBoxKey, InletsBoxKey, InletButtonKey)
import Cli.Keys as Key
import Cli.Style as Style
import Cli.State (State, Link, OutletIndex(..), InletIndex(..))
import Cli.State.NwWraper (wrapN, unwrapN)
import Cli.Components.Link as Link
import Cli.Components.NodeBox.InletButton as InletButton

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Network2 as Network
import Noodle.Family.Def as Family

import Toolkit.Hydra2 (Instances, State) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InletButton.widthN + 1) * count


type KeysMap = Map Id.InputR InletButtonKey


component
    :: forall f state is os
    -- :: forall id r f state fs iis rli is rlo os repr_is repr_os
    --  . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    -- => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    -- => FromToReprRow rli is Hydra.WrapRepr
    -- => FromToReprRow rlo os Hydra.WrapRepr
    -- => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -- => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
     . Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> InletsBoxKey
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Array (Maybe Hydra.WrapRepr /\ Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -> KeysMap /\ C.Blessed State
component curPatchId curPatch nextNodeBox nextInletsBox family _ is =
    Map.empty /\ (
    B.box nextInletsBox
        [ Box.width $ width $ Array.length is
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inletHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inletHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.inletsOutlets
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "inlet"
                inletSelected <- List.selected ~< nextInletsBox
                liftEffect $ Console.log $ show inletSelected
        -}
        ]
        $ mapWithIndex mapF
        $ fillKeys is )
    where
        fillKeys =
            Tuple.snd <<< foldr foldF (Key.inletButton /\ [])
        foldF hinnr (prevKey /\ pairs) =
            let nextKey = Key.next prevKey
            in nextKey /\ ((nextKey /\ hinnr) : pairs)
        mapF idx (nextKey /\ (maybeRepr /\ hiinr)) =
            -- FIXME: either pass Repr inside `withInputInNodeMRepr` or get rid of `HoldsInputInNodeMRepr` completely since we have ways to get Repr from outside using folds
            Node.withInputInNodeMRepr hiinr (InletButton.component nextKey curPatchId curPatch nextNodeBox idx maybeRepr)
