module Cli.Components.NodeBox.OutletsBox where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map

import Signal (Signal)
import Signal as Signal

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys (OutletButtonKey)
import Cli.Style as Style
import Cli.State (State, LastKeys)
import Cli.Components.NodeBox.OutletButton as OutletButton

import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel) as T
import Noodle.Repr (class HasFallback, fallback)
-- import Noodle.Family.Def as Family

-- import Cli.Components.NodeBox.HasBody (class HasEditor)

import Noodle.Raw.Node (OutletsValues, OrderedOutletsValues)


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (OutletButton.widthN + 1) * count


type KeysMap = Map Id.InletR OutletButtonKey -- TODO: store in State?


component
    :: forall tk pstate fs repr m
     . HasFallback repr
    => T.At T.ChannelLabel repr
    => Offset
    -> Patch pstate fs repr m
    -> LastKeys
    -> Id.FamilyR
    -> Signal (OutletsValues repr)
    -> OrderedOutletsValues repr
    -> KeysMap /\ C.Blessed (State tk pstate fs repr m)
component offsetY curPatch keys family oReprSignal outlets =
    Map.empty /\
    B.box keys.outletsBox
        [ Box.width $ width $ Map.size outlets -- * OutletButton.widthN
        , Box.height $ Dimension.px 1
        , Box.top offsetY
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
        (makeOutletButton <$> Array.zip keysArray outletsArr)
    where
        outletsArr = Map.toUnfoldable outlets
        keysArray :: Array OutletButtonKey
        keysArray = NK.nestChain keys.nodeBox $ Array.length outletsArr
        makeOutletButton :: (OutletButtonKey /\ ((Int /\ Id.OutletR) /\ repr)) -> C.Blessed (State tk pstate fs repr m)
        makeOutletButton (buttonKey /\ ((idx /\ outletR ) /\ repr)) =
            OutletButton.component curPatch buttonKey keys.infoBox keys.nodeBox outletR idx (Just repr)
                $ Signal.filterMap (Map.lookup outletR) (fallback :: repr)
                $ oReprSignal
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