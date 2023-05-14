module Cli.Components.NodeBox where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console as Console

import Control.Monad.State as State

import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append) as S
import Data.Int (floor, toNumber)
import Data.Ord (abs)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (class Newtype, unwrap)
import Data.Map (Map)
import Data.Map as Map
import Data.Array ((:), (!!))
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.List (toUnfoldable, length) as List
import Data.KeyHolder as KH
import Record.Extra (class Keys, keys) as Record
import Unsafe.Coerce (unsafeCoerce)
import Data.String as String
import Data.Repr (Repr, class FromRepr, class ToRepr, class FromToReprRow, toRepr, fromRepr)

import Blessed ((>~), (~<))
import Blessed (exit) as Blessed
import Blessed as B

import Blessed.Core.Border as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Key (Key) as C
import Blessed.Core.Key as Key
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Orientation as Orientation

import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)
import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp, BlessedOpM)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.Emitter (class Fires) as E

import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Element.Property as Element
import Blessed.UI.Base.Element.PropertySet as Element
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event as List
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.List.Property as List
import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Lists.ListBar.Option as ListBar
import Blessed.UI.Lists.ListBar.Method as ListBar
import Blessed.UI.Boxes.Line.Option as Line
import Blessed.UI.Boxes.Line.Event as Line
import Blessed.UI.Forms.Button as Button
import Blessed.UI.Forms.Button.Option as Button
import Blessed.UI.Forms.Button.Event as Button
-- import Blessed.UI.Line.Li ()

import Noodle.Id as Id
import Noodle.Toolkit3 as Toolkit
import Noodle.Network2 as Network
import Noodle.Network2 (Network) as Noodle
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch) as Noodle
import Noodle.Node2 as Node
import Noodle.Node2 (Node) as Noodle
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Repr (nodeToRepr, nodeToMapRepr, Repr(..), class HasRepr, class ToReprHelper) as R


import Cli.App as Cli
import Cli.Keys (NodeBoxKey)
import Cli.Keys as Key
import Cli.Palette (palette)
import Cli.Style as Style
import Cli.State (initial, patchIdFromIndex) as State
import Cli.State (State, Link(..), InletIndex(..), OutletIndex(..))
import Cli.State.NwWraper (Network, wrapN, unwrapN, withNetwork)
import Cli.Components.Link as Link
import Cli.Components.PatchesBar as PatchesBar
import Cli.Components.AddPatch as AddPatch
import Cli.Components.NodeBox.InletsBar as InletsBar

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.BlessedRepr as Hydra


fromFamily
    :: forall f state fs iis rli is rlo os repr_is repr_os
     . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.BlessedRepr state
    => FromToReprRow rli is Hydra.BlessedRepr
    => FromToReprRow rlo os Hydra.BlessedRepr
    => Node.TestNodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.BlessedRepr)
    => Node.TestNodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)
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
        let (issss :: Array (Node.HoldsInputInNodeMRepr Effect Hydra.BlessedRepr)) = Node.orderedInputsMRepr node
        let (ossss :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)) = Node.orderedOutputsMRepr node
        let (issss1 :: Array (Node.HoldsInputInNodeMRepr Effect Hydra.BlessedRepr)) = Node.orderedNodeInputsTest' node
        let (ossss1 :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)) = Node.orderedNodeOutputsTest' node
        -- let (ossss1 :: Array (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)) = Node.orderedNodeOutputsTest' node
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
        repr <- R.nodeToRepr (Proxy :: _ Effect) (R.Repr :: _ Hydra.BlessedRepr) node
        -- state <- State.get
        pure { nextPatch, node, inputs, is, iss, iss2, isss, issss, issss1, os, oss, oss2, osss, ossss, ossss1, outputs, nodes, repr }

    -- let is /\ os = Node.shapeH rec.node
    let is /\ os = rec.issss1 /\ rec.ossss1
    liftEffect $ Console.log $ "is" <> (show $ List.length rec.is)
    liftEffect $ Console.log $ "os" <> (show $ List.length rec.os)
    liftEffect $ Console.log $ "iss" <> (show $ Array.length rec.iss)
    liftEffect $ Console.log $ "oss" <> (show $ Array.length rec.oss)
    liftEffect $ Console.log $ "iss2-" <> (show $ Array.length rec.iss2)
    liftEffect $ Console.log $ "oss2-" <> (show $ Array.length rec.oss2)
    -- liftEffect $ Console.log $ "isss" <> (show $ Array.length rec.isss)
    -- liftEffect $ Console.log $ "osss" <> (show $ Array.length rec.osss)
    -- liftEffect $ Console.log $ "issss" <> (show $ Array.length rec.issss)
    -- liftEffect $ Console.log $ "ossss" <> (show $ Array.length rec.ossss)
    liftEffect $ Console.log $ "issss1" <> (show $ Array.length rec.issss1)
    liftEffect $ Console.log $ "ossss1" <> (show $ Array.length rec.ossss1)
    let repr = rec.repr
    let nodeId = Node.id rec.node
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
        inletsBarN = InletsBar.component curPatchId curPatch nextNodeBox nextInletsBar family def is
        nextNodeBoxN =
            B.box nextNodeBox
                [ Box.draggable true
                , Box.top top
                , Box.left left
                , Box.width $ Dimension.px 25
                , Box.height $ Dimension.px 5
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

    let
        outletHandler :: forall f nstate o dout is os os'. IsSymbol f => Id.HasOutput o dout os' os => ToRepr dout Hydra.BlessedRepr => FromRepr Hydra.BlessedRepr dout => Int -> Proxy dout -> Noodle.Node f nstate is os Effect -> Id.Output o -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-outlets-bar" State
        outletHandler index pdout node output =
            Id.reflect output /\ [] /\ \_ _ -> do
                -- liftEffect $ Console.log $ "handler " <> oname
                State.modify_
                    (_
                        { lastClickedOutlet =
                            Just
                                { index, subj : Id.reflect output, nodeKey : nextNodeBox, nodeId : Id.holdNodeId nodeId, outputId : Node.holdOutputInNodeMRepr pdout node output, node : nodeHolder } })
                -- onOutletSelect nodeId output nextNodeBox idx (Id.reflect output)
        outletsBarN =
            B.listbar nextOutletsBar
                [ Box.width $ Dimension.percents 90.0
                , Box.height $ Dimension.px 1
                , Box.top $ Offset.px 2
                , Box.left $ Offset.px 0
                -- , List.items os
                , ListBar.commands $ mapWithIndex (\idx hoinr -> Node.withOutputInNodeMRepr hoinr (outletHandler idx)) os
                -- , ListBar.commands $ mapWithIndex outletHandler $ Id.reflect' <$> os
                -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex outletHandler $ os
                , List.mouse true
                , List.keys true
                , Style.inletsOutlets
                {- , Core.on ListBar.Select
                    \_ _ -> do
                        liftEffect $ Console.log "outlet"
                        outletSelected <- List.selected ~< nextOutletsBar
                        liftEffect $ Console.log $ show outletSelected
                -}
                ]
                [
                ]


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
