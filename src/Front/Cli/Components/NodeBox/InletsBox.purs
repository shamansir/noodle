module Cli.Components.NodeBox.InletsBox where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple as Tuple
import Signal (Signal)
import Signal as Signal

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys (InletButtonKey)
import Cli.Style as Style
import Cli.State (State, LastKeys)
import Cli.Components.NodeBox.InletButton as InletButton

import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Wiring (class Wiring)
-- import Noodle.Family.Def as Family

-- import Cli.Components.NodeBox.HasBody (class HasEditor)

import Noodle.Raw.Node (InletsValues, OrderedInletsValues)


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InletButton.widthN + 1) * count


component
    :: forall tk pstate fs repr m
     . Wiring m
    => HasFallback chrepr
    => T.At T.ChannelLabel repr
    => T.At T.StatusLine repr
    => Id.PatchR
    -> LastKeys
    -> Id.FamilyR -> Id.NodeR
    -> Signal (InletsValues repr)
    -> OrderedInletsValues repr
    -> Map Id.InletR InletButtonKey
    /\ C.Blessed (State tk pstate fs repr m)
component patchR keys familyR nodeR iReprSignal inlets =
    inletsKeys /\
    B.box keys.inletsBox
        [ Box.width $ width $ Map.size inlets -- * InletButton.widthN
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        -- , List.items is

        -- , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inputHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inputHandler $ is
        -- , List.mouse true
        -- , List.keys true
        -- , ListBar.autoCommandKeys true
        , Style.inletsOutlets
        {- , Core.on ListBar.Select
            \_ _ -> do
                CC.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                CC.log $ show inputSelected
        -}
        ]
        inletsButtons
    where
        inletsArr :: Array ((Int /\ Id.InletR) /\ repr)
        inletsArr = Map.toUnfoldable inlets
        keysArray :: Array InletButtonKey
        keysArray = NK.nestChain keys.nodeBox $ Array.length inletsArr
        inletsButtonsWithKeys :: Array ((Id.InletR /\ InletButtonKey) /\ C.Blessed (State tk pstate fs repr m))
        inletsButtonsWithKeys = makeInletButton <$> Array.zip keysArray inletsArr
        makeInletButton :: (InletButtonKey /\ ((Int /\ Id.InletR) /\ repr)) -> (Id.InletR /\ InletButtonKey) /\ C.Blessed (State tk pstate fs repr m)
        makeInletButton (buttonKey /\ ((idx /\ inletR) /\ repr)) =
            (inletR /\ buttonKey)
            /\
            ( InletButton.component patchR buttonKey keys.nodeBox keys.infoBox familyR nodeR inletR idx (Just repr)
            $ Signal.filterMap (Map.lookup inletR) (fallback :: repr)
            $ iReprSignal
            )
        inletsButtons :: Array (C.Blessed (State tk pstate fs repr m))
        inletsButtons = Tuple.snd <$> inletsButtonsWithKeys
        inletsKeys :: Map Id.InletR InletButtonKey
        inletsKeys = Map.fromFoldable (Tuple.fst <$> inletsButtonsWithKeys)