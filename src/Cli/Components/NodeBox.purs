module Cli.Components.NodeBox where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Control.Monad.State as State

import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(..))
import Data.Map as Map
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (length) as List
import Data.KeyHolder as KH
import Data.Repr (class FromToReprRow, class ToReprRow)

import Signal (Signal, (~>))
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Channel

import Blessed ((>~))
import Blessed as B
import Blessed.Tagger (fg, s)
import Blessed.Tagger (render) as T

import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style

import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.NodeKey as NodeKey

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
-- import Blessed.UI.Line.Li ()

import Noodle.Id as Id
import Noodle.Toolkit3 as Toolkit
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch) as Noodle
import Noodle.Node2 as Node
import Noodle.Node2 (Node) as Noodle
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Repr
    ( class ToReprHelper, class ToReprFoldToMapsHelper
    , Repr(..)
    , nodeToRepr, nodeToMapRepr
    , subscribeReprChanges, subscribeReprMapChanges
    ) as R
import Noodle.Node2.MapsFolds.Flatten as R


import Cli.Keys (NodeBoxKey)
import Cli.Keys (mainScreen, patchBox) as Key
import Cli.Palette (palette)
import Cli.State (State)
import Cli.Components.Link as Link
import Cli.Components.NodeBox.InletsBar as InletsBar
import Cli.Components.NodeBox.OutletsBar as OutletsBar

import Toolkit.Hydra2 (class HasNodesOf, Instances, State, Toolkit) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Repr.Info (InfoRepr) as Hydra


fromFamily
    :: forall f state fs iis rli is rlo os repr_is repr_os
     . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    => R.ToReprFoldToMapsHelper f is rli os rlo Hydra.WrapRepr state
    => FromToReprRow rli is Hydra.WrapRepr
    => FromToReprRow rlo os Hydra.WrapRepr
    => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    => Patch.Id
    -> Noodle.Patch Hydra.State (Hydra.Instances Effect)
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Hydra.Toolkit Effect
    -> BlessedOpM State Effect _
fromFamily curPatchId curPatch family def tk = do
    state <- State.get

    let nextNodeBox = NodeKey.next state.lastNodeBoxKey
    let nextInletsBar = NodeKey.next state.lastInletsBarKey
    let nextOutletsBar = NodeKey.next state.lastOutletsBarKey

    let top = Offset.px $ state.lastShiftX + 2
    let left = Offset.px $ 16 + state.lastShiftY + 2

    rec <- liftEffect $ do
        (node :: Noodle.Node f state is os Effect) <- Toolkit.spawn tk family
        (inputs :: Record is) <- Node.inputs node
        (outputs :: Record os) <- Node.outputs node
        let (iss :: Array Id.HoldsInput) = KH.orderedKeys' (Proxy :: _ Id.Input) (Node.inputsOrder node) inputs
        let (oss :: Array Id.HoldsOutput) = KH.orderedKeys' (Proxy :: _ Id.Output) (Node.outputsOrder node) outputs
        let (iss2 :: Array Node.HoldsInputInNode) = Node.orderedInputs node
        let (oss2 :: Array Node.HoldsOutputInNode) = Node.orderedOutputs node
        let (isss :: Array (Node.HoldsInputInNodeM Effect)) = Node.orderedInputsM node
        let (osss :: Array (Node.HoldsOutputInNodeM Effect)) = Node.orderedOutputsM node
        let (issss :: Array (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedInputsMRepr node
        let (ossss :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedOutputsMRepr node
        let (issss1 :: Array (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeInputsTest' node
        let (ossss1 :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeOutputsTest' node
        -- let (ossss1 :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)) = Node.orderedNodeOutputsTest' node
        -- let (osss :: Array Id.HoldsOutput) = KH.orderedKeys' (Proxy :: _ Id.Output) (Node.outputsOrder node) outputs

        -- TODO
        -- let (issh :: Array Node.HoldsInputInNode) = KH.orderedKeys1' (Proxy :: _ Id.Input) node (Node.inputsOrder node) inputs
        -- let (ossh :: Array Node.HoldsOutputInNode) = KH.orderedKeys1' (Proxy :: _ Id.Output) node (Node.outputsOrder node) outputs
        let is /\ os = Node.shape node
        -- Console.log $ String.joinWith ":" $ List.toUnfoldable $ show <$> Record.keys inputs
        -- Console.log $ String.joinWith ":" $ List.toUnfoldable $ show <$> Record.keys outputs
        let nextPatch = Patch.registerNode node (curPatch :: Noodle.Patch Hydra.State (Hydra.Instances Effect))
        -- let (nodeHolder :: Patch.HoldsNode' Hydra.State (Hydra.Instances Effect) Effect) = Patch.holdNode' nextPatch node
        -- let nextPatch' = Hydra.spawnAndRegister curPatch familyR
        let (nodes :: Array (Noodle.Node f state is os Effect)) = Patch.nodesOf family nextPatch
        repr <- R.nodeToRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node
        mapRepr <- R.nodeToMapRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.WrapRepr) node
        let (updates' :: Signal (R.NodeLineRec f Hydra.WrapRepr repr_is repr_os)) = R.subscribeReprChanges (R.Repr :: _ Hydra.WrapRepr) node
        let (updates :: Signal (R.NodeLineMap Hydra.WrapRepr)) = R.subscribeReprMapChanges (R.Repr :: _ Hydra.WrapRepr) node
        -- state <- State.get
        pure { nextPatch, node, inputs, is, iss, iss2, isss, issss, issss1, os, oss, oss2, osss, ossss, ossss1, outputs, nodes, repr, mapRepr, updates }

    -- let is /\ os = Node.shapeH rec.node
    let is /\ os = rec.issss1 /\ rec.ossss1
    -- liftEffect $ Console.log $ "is" <> (show $ List.length rec.is)
    -- liftEffect $ Console.log $ "os" <> (show $ List.length rec.os)
    -- liftEffect $ Console.log $ "iss" <> (show $ Array.length rec.iss)
    -- liftEffect $ Console.log $ "oss" <> (show $ Array.length rec.oss)
    -- liftEffect $ Console.log $ "iss2-" <> (show $ Array.length rec.iss2)
    -- liftEffect $ Console.log $ "oss2-" <> (show $ Array.length rec.oss2)
    -- -- liftEffect $ Console.log $ "isss" <> (show $ Array.length rec.isss)
    -- -- liftEffect $ Console.log $ "osss" <> (show $ Array.length rec.osss)
    -- -- liftEffect $ Console.log $ "issss" <> (show $ Array.length rec.issss)
    -- -- liftEffect $ Console.log $ "ossss" <> (show $ Array.length rec.ossss)
    -- liftEffect $ Console.log $ "issss1" <> (show $ Array.length rec.issss1)
    -- liftEffect $ Console.log $ "ossss1" <> (show $ Array.length rec.ossss1)
    let repr = rec.repr
    let mapRepr = rec.mapRepr
    let nodeId = Node.id rec.node
    let updates = rec.updates
    let (node :: Noodle.Node f state is os Effect) = rec.node
    let (nodeHolder :: Patch.HoldsNode Effect) = Patch.holdNode rec.nextPatch node

    -- TODO: probably use Repr to create inlet bars and outlet bars, this way using Input' / Output' instances, we will probably be able to connect things
    --       or not Repr but some fold over inputs / outputs shape
    --       but the question remains: when we have some selected input for the receiving node in the handler, wherefrom do we get the node id of the output?
    --       we have the family encoded as symbol and hash of the is the thing that changes in real-time
    --       so we need to recreate the family. In case of Hydra, we have access to families' symbols but also by symbols.
    --       we have `lastClickedOtlet` in the state.
    --       Maybe try using `Exists` as we're sure the Node Family exists but don't want to parametrize `State` type with it.

    -- let is /\ os = Record.keys (rec.inputs :: Record is) /\ Record.keys (rec.outputs :: Record os)

    let
        inletsBarN =
            InletsBar.component curPatchId curPatch nextNodeBox nextInletsBar family def is
        outletsBarN =
            OutletsBar.component nodeHolder nextNodeBox nextOutletsBar os
        nextNodeBoxN =
            B.box nextNodeBox
                [ Box.draggable true
                , Box.top top
                , Box.left left
                , Box.width $ Dimension.px 25
                , Box.height $ Dimension.px 5
                , Box.label $ T.render $ fg "red" $ s $ Id.reflect family
                , Box.tags true
                , Box.border
                    [ Border.type_ Border._line
                    , Border.fg palette.nodeBoxBorder
                    , Border.ch $ Border.fill ':'
                    ]
                , Box.style
                    [ Style.focus
                        [ ES.border
                            [ Border.fg palette.nodeListSelFg
                            ]
                        ]
                    ]
                , Core.on Element.Move $ onMove nextNodeBox -- FIXME: onNodeMove receives wrong `NodeKey` in the handler, probably thanks to `proxies` passed around
                ]
                [ ]

    Key.patchBox >~ Node.append nextNodeBoxN
    nextNodeBox >~ Node.append inletsBarN
    nextNodeBox >~ Node.append outletsBarN

    State.modify_ (_
        { lastShiftX = state.lastShiftX + 1
        , lastShiftY = state.lastShiftY + 1
        , lastNodeBoxKey = nextNodeBox
        , lastInletsBarKey = nextInletsBar
        , lastOutletsBarKey = nextOutletsBar
        } )

    Key.mainScreen >~ Screen.render

    pure { nextNodeBoxN, inletsBarN, outletsBarN }


onMove :: NodeBoxKey -> NodeBoxKey → EventJson → BlessedOp State Effect
onMove nodeKey _ _ = do
    state <- State.get
    let rawNk = NodeKey.rawify nodeKey
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) Link.update
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) Link.update
