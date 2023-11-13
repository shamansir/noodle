module Cli.Components.NodeBox.InputsBox where

import Prelude

import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe)
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Signal (Signal)
import Signal as Signal

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys (NodeBoxKey, InputsBoxKey, InputButtonKey, InfoBoxKey)
import Cli.Style as Style
import Cli.State (State)
import Cli.Components.NodeBox.InputButton as InputButton

import Noodle.Id as Id
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Family.Def as Family

import Tookit.Hydra (Instances, State) as Hydra
import Tookit.Hydra.Repr.Wrap (WrapRepr) as Hydra
import Tookit.Hydra.Family.Render.Cli (CliD) as Hydra
-- import Cli.Components.NodeBox.HasBody (class HasEditor)


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InputButton.widthN + 1) * count


type KeysMap = Map Id.InputR InputButtonKey -- TODO: store in State?


component
    :: forall f
    -- :: forall id r f state fs iis rli is rlo os repr_is repr_os
    --  . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    -- => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    -- => FromToReprRow rli is Hydra.WrapRepr
    -- => FromToReprRow rlo os Hydra.WrapRepr
    -- => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -- => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
     . {- HasEditor (Hydra.CliD din) (Id.Input i) (Noodle.Node f nstate is os Effect) din Effect
    => -} Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> InfoBoxKey
    -> InputsBoxKey
    -> Id.Family f
    -> Signal (Id.InputR -> Maybe Hydra.WrapRepr)
    -> Array (Maybe Hydra.WrapRepr /\ Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -> KeysMap /\ C.Blessed State
component curPatchId curPatch nextNodeBox nextInfoBox nextInputsBox family iReprSignal is =
    inputsKeysMap /\
    B.box nextInputsBox
        [ Box.width $ width $ Array.length is
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
        inputsButtons
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