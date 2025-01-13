module Cli.Components.NodeBox.OutletsBox where

import Prelude

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
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.ChRepr (ValueInChannel)
import Noodle.Repr.ChRepr (_reportMissingKey) as ChRepr
-- import Noodle.Family.Def as Family

-- import Cli.Components.NodeBox.HasBody (class HasEditor)

import Noodle.Raw.Node (OutletsValues, OrderedOutletsValues)


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (OutletButton.widthN + 1) * count


component
    :: forall tk pstate fs strepr chrepr m
     . HasFallback chrepr
    => T.At T.StatusLine chrepr
    => T.At T.ChannelLabel chrepr
    => Offset
    -> LastKeys
    -> Id.FamilyR -> Id.NodeR
    -> Signal (OrderedOutletsValues chrepr)
    -> OrderedOutletsValues chrepr
    -> Map Id.OutletR OutletButtonKey
    /\ C.Blessed (State tk pstate fs strepr chrepr m)
component offsetY keys familyR nodeR oReprSignal outlets =
    outletsKeys /\
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
        , Style.inletsOutlets
        {- , Core.on ListBar.Select
            \_ _ -> do
                CC.log "input"
                inputSelected <- List.selected ~< nextInputsBox
                CC.log $ show inputSelected
        -}
        ]
        outletsButtons
    where
        outletsArr :: Array ((Int /\ Id.OutletR) /\ ValueInChannel chrepr)
        outletsArr = Map.toUnfoldable outlets
        keysArray :: Array OutletButtonKey
        keysArray = NK.nestChain keys.nodeBox $ Array.length outletsArr
        outletsButtonsWithKeys :: Array ((Id.OutletR /\ OutletButtonKey) /\ C.Blessed (State tk pstate fs strepr chrepr m))
        outletsButtonsWithKeys = makeOutletButton <$> Array.zip keysArray outletsArr
        makeOutletButton :: (OutletButtonKey /\ ((Int /\ Id.OutletR) /\ ValueInChannel chrepr)) -> (Id.OutletR /\ OutletButtonKey) /\ C.Blessed (State tk pstate fs strepr chrepr m)
        makeOutletButton (buttonKey /\ ((idx /\ outletR) /\ vicRepr)) =
            (outletR /\ buttonKey)
            /\
            ( OutletButton.component buttonKey keys.nodeBox keys.infoBox familyR nodeR outletR idx vicRepr
            $ map (Map.lookup outletR >>> (ChRepr._reportMissingKey $ Id.outletRName outletR))
            $ map (Map.mapKeys Tuple.snd)
            $ oReprSignal
            )
        outletsButtons :: Array (C.Blessed (State tk pstate fs strepr chrepr m))
        outletsButtons = Tuple.snd <$> outletsButtonsWithKeys
        outletsKeys :: Map Id.OutletR OutletButtonKey
        outletsKeys = Map.fromFoldable (Tuple.fst <$> outletsButtonsWithKeys)