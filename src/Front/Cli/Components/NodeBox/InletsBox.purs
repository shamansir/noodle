module Cli.Components.NodeBox.InletsBox where

import Prelude

import Effect.Ref (Ref)

import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map
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
import Cli.Class.CliRenderer (class CliEditor)

import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Wiring (class Wiring)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (_reportMissingKey) as ViC
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ValueEncode) as Ndf
-- import Noodle.Family.Def as Family

-- import Cli.Components.NodeBox.HasBody (class HasEditor)

import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (InletsValues, OrderedInletsValues)


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (InletButton.widthN + 1) * count


component
    :: forall tk pstate fs strepr chrepr m
     . Wiring m
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    => CliEditor tk chrepr
    => T.At T.ChannelLabel chrepr
    => T.At T.StatusLine chrepr
    => Ndf.ValueEncode chrepr
    => Ref (State _ tk pstate fs strepr chrepr m)
    -> Id.PatchR
    -> LastKeys
    -> Raw.Node strepr chrepr m
    -> Signal (OrderedInletsValues chrepr)
    -> OrderedInletsValues chrepr
    -> Map Id.InletR InletButtonKey
    /\ C.Blessed (State _ tk pstate fs strepr chrepr m)
component stateRef patchR keys rawNode iReprSignal inlets =
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
        inletsArr :: Array ((Int /\ Id.InletR) /\ ValueInChannel chrepr)
        inletsArr = Map.toUnfoldable inlets
        keysArray :: Array InletButtonKey
        keysArray = NK.nestChain keys.nodeBox $ Array.length inletsArr
        inletsButtonsWithKeys :: Array ((Id.InletR /\ InletButtonKey) /\ C.Blessed (State _ tk pstate fs strepr chrepr m))
        inletsButtonsWithKeys = makeInletButton <$> Array.zip keysArray inletsArr
        makeInletButton :: (InletButtonKey /\ ((Int /\ Id.InletR) /\ ValueInChannel chrepr)) -> (Id.InletR /\ InletButtonKey) /\ C.Blessed (State _ tk pstate fs strepr chrepr m)
        makeInletButton (buttonKey /\ ((idx /\ inletR) /\ vicRepr)) =
            (inletR /\ buttonKey)
            /\
            ( InletButton.component stateRef patchR buttonKey keys.nodeBox keys.infoBox rawNode inletR idx vicRepr
            $ map (Map.lookup inletR >>> (ViC._reportMissingKey $ Id.inletRName inletR))
            $ map (Map.mapKeys Tuple.snd)
            $ iReprSignal
            )
        inletsButtons :: Array (C.Blessed (State _ tk pstate fs strepr chrepr m))
        inletsButtons = Tuple.snd <$> inletsButtonsWithKeys
        inletsKeys :: Map Id.InletR InletButtonKey
        inletsKeys = Map.fromFoldable (Tuple.fst <$> inletsButtonsWithKeys)