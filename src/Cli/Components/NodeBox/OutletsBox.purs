module Cli.Components.NodeBox.OutletsBox where

import Prelude



import Control.Monad.State as State

import Effect (Effect)
import Type.Proxy (Proxy)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Array ((:))
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map

import Signal (Signal)

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Key (Key) as C
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedSubj (ListBar)
import Blessed.Internal.NodeKey (nestChain) as NK
import Blessed.Internal.NodeKey (next) as Key

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option (keys, mouse) as List
import Blessed.UI.Lists.ListBar.Option (commands) as ListBar
import Blessed.Internal.Core as Core

import Cli.Keys (NodeBoxKey, OutletsBoxKey, OutletButtonKey, InfoBoxKey)
import Cli.Keys (outletButton) as Key
import Cli.Style as Style
import Cli.State (State)
import Cli.Components.NodeBox.OutletButton as OutletButton

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 as Patch

import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (OutletButton.widthN + 1) * count


type KeysMap = Map Id.OutputR OutletButtonKey


component
    -- forall id r f state fs iis rli is rlo os repr_is repr_os
    -- . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    -- => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    -- => FromToReprRow rli is Hydra.WrapRepr
    -- => FromToReprRow rlo os Hydra.WrapRepr
    -- => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -- => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    :: Offset
    -> Patch.HoldsNode Effect
    -> NodeBoxKey
    -> InfoBoxKey
    -> OutletsBoxKey
    -> Signal (Id.OutputR -> Maybe Hydra.WrapRepr)
    -> Array (Maybe Hydra.WrapRepr /\ Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    -> KeysMap /\ C.Blessed State
component topOffset nodeHolder nextNodeBox nextInfoBox nextOutletsBox oReprSignal os =
    outputsKeysMap /\
    B.box nextOutletsBox
        [ Box.width $ width $ Array.length os
        , Box.height $ Dimension.px 1
        , Box.top topOffset
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
        outputsButtons
    where
        extractOutput :: Id.OutputR -> Signal (Id.OutputR -> Maybe Hydra.WrapRepr) -> Signal (Maybe Hydra.WrapRepr)
        extractOutput outputR = map ((#) outputR)
        keysArray :: Array OutletButtonKey
        keysArray = NK.nestChain nextNodeBox $ Array.length os
        outputsKeysMap =
            Map.fromFoldable $ toKeyPair <$> Array.zip keysArray os
        toKeyPair (buttonKey /\ (_ /\ hoinr)) =
            Node.withOutputInNodeMRepr hoinr \_ _ outputId -> Id.outputR outputId
            /\ buttonKey
        outputsButtons =
            mapWithIndex mapF $ Array.zip keysArray os
        mapF idx (buttonKey /\ (maybeRepr /\ hoinr)) =
            -- FIXME: either pass Repr inside `withInputInNodeMRepr` or get rid of `HoldsInputInNodeMRepr` completely since we have ways to get Repr from outside using folds
            Node.withOutputInNodeMRepr hoinr
                (\pdout node output ->
                    OutletButton.component buttonKey nextInfoBox nodeHolder nextNodeBox nextOutletsBox idx maybeRepr (extractOutput (Id.outputR output) oReprSignal) pdout node output
                )


{-}
        -- $ mapWithIndex (\idx hoinr -> Node.withOutputInNodeMRepr hoinr (OutletButton.component nodeHolder nextNodeBox nextOutletsBox idx)) os
        $ mapWithIndex mapF
        $ fillKeys os )
    where
        fillKeys =
            Tuple.snd <<< foldr foldF (Key.outletButton /\ [])
        foldF hoinr (prevKey /\ pairs) =
            let nextKey = Key.next prevKey
            in nextKey /\ ((nextKey /\ hoinr) : pairs)
        mapF idx (nextKey /\ (maybeRepr /\ hoinr)) =
            -- FIXME: either pass Repr inside `withInputInNodeMRepr` or get rid of `HoldsInputInNodeMRepr` completely since we have ways to get Repr from outside using folds
            Node.withOutputInNodeMRepr hoinr (OutletButton.component nextKey nodeHolder nextNodeBox nextOutletsBox idx maybeRepr)
-}
