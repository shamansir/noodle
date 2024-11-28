module Cli.Components.NodeBox.InletsBox where

import Prelude

import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Signal (Signal, (~>))
import Signal as Signal

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys (NodeBoxKey, InletsBoxKey, InletButtonKey, InfoBoxKey)
import Cli.Style as Style
import Cli.State (State, LastKeys)
import Cli.Components.NodeBox.InletButton as InletButton

import Noodle.Id as Id
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel) as T
import Noodle.Ui.Cli.Palette.Mark (class Mark)
import Noodle.Repr (class HasFallback, fallback)
-- import Noodle.Family.Def as Family

-- import Cli.Components.NodeBox.HasBody (class HasEditor)

import Noodle.Raw.Node (InletsValues, OrderedInletsValues)


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InletButton.widthN + 1) * count


type KeysMap = Map Id.InletR InletButtonKey -- TODO: store in State?


component
    :: forall tk pstate fs repr m
    -- :: forall id r f state fs iis rli is rlo os repr_is repr_os
    --  . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    -- => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    -- => DataFromToReprRow rli is Hydra.WrapRepr
    -- => DataFromToReprRow rlo os Hydra.WrapRepr
    -- => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -- => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    {- HasEditor (Hydra.CliD din) (Id.Input i) (Noodle.Node f nstate is os Effect) din Effect
    => -}
     . HasFallback repr
    => Mark repr
    => T.At T.ChannelLabel repr
    => Patch pstate fs repr m
    -> LastKeys
    -> Id.FamilyR
    -> Signal (InletsValues repr)
    -> OrderedInletsValues repr
    -> KeysMap /\ C.Blessed (State tk pstate fs repr m)
component curPatch keys family iReprSignal inlets =
    Map.empty /\
    B.box keys.inletsBox
        [ Box.width $ width $ Map.size inlets
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        -- , List.items is


        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inputHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inputHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.inputsOutputs
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                liftEffect $ Console.log $ show inputSelected
        -}
        ]
        (makeInletButton <$> Array.zip keysArray inletsArr)
    where
        inletsArr = Map.toUnfoldable inlets
        keysArray :: Array InletButtonKey
        keysArray = NK.nestChain keys.nodeBox $ Array.length inletsArr
        makeInletButton :: (InletButtonKey /\ ((Int /\ Id.InletR) /\ repr)) -> C.Blessed (State tk pstate fs repr m)
        makeInletButton (buttonKey /\ ((idx /\ inletR ) /\ repr)) =
            InletButton.component curPatch buttonKey keys.infoBox keys.nodeBox inletR idx (Just repr)
                $ Signal.filterMap (Map.lookup inletR) (fallback :: repr)
                $ iReprSignal
    {- REM
    where
        extractInput :: Id.InputR -> Signal (Id.InputR -> Maybe Hydra.WrapRepr) -> Signal (Maybe Hydra.WrapRepr)
        extractInput inputR = map ((#) inputR)
        keysArray :: Array InputButtonKey
        keysArray = NK.nestChain nextNodeBox $ Array.length is
        inputsKeysMap =
            Map.fromFoldable $ toKeyPair <$> Array.zip keysArray is
        toKeyPair (buttonKey /\ (_ /\ hiinr)) =
            Node.withInputInNodeMRepr hiinr \_ _ inputId -> Id.inputR inputId
            /\ buttonKey
        inputsButtons =
            mapWithIndex mapF $ Array.zip keysArray is
        mapF idx (buttonKey /\ (maybeRepr /\ hiinr)) =
            -- FIXME: either pass Repr inside `withInputInNodeMRepr` or get rid of `HoldsInputInNodeMRepr` completely since we have ways to get Repr from outside using folds
            Node.withInputInNodeMRepr hiinr
                (\pdin node input -> do
                    InputButton.component buttonKey nextInfoBox curPatchId curPatch nextNodeBox idx maybeRepr (extractInput (Id.inputR input) iReprSignal) pdin node input
                )
    -}