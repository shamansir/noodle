module Cli.Components.NodeBox.InletsBar where

import Prelude

import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (Repr, class FromRepr, class ToRepr, class FromToReprRow, toRepr, fromRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Type.Row (type (+))

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

import Blessed.Internal.Core (Blessed, Node, NodeAnd, run, runAnd) as C
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp, BlessedOpM)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)

import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Element.Property as Element
import Blessed.UI.Base.Element.PropertySet as Element
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
import Blessed.Internal.Core as Core

import Cli.Keys (NodeBoxKey, InletsBarKey)
import Cli.Keys as Key
import Cli.Style as Style
import Cli.State (State, Link(..), OutletIndex(..), InletIndex(..))
import Cli.State.NwWraper (wrapN, unwrapN)
import Cli.Components.Link as Link

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Network2 as Network
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Repr (nodeToRepr, nodeToMapRepr, Repr(..), class HasRepr, class ToReprHelper) as R

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.BlessedRepr as Hydra


component
    :: forall id r f state fs iis rli is rlo os repr_is repr_os
     . {- Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.BlessedRepr state
    => FromToReprRow rli is Hydra.BlessedRepr
    => FromToReprRow rlo os Hydra.BlessedRepr
    => Node.TestNodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.BlessedRepr)
    => Node.TestNodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr)
    => -} Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> InletsBarKey
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Array (Node.HoldsInputInNodeMRepr Effect Hydra.BlessedRepr)
    -> _
    -- -> C.Node ListBar id ( ListBar.OptionsRow + r ) state
component curPatchId curPatch nextNodeBox nextInletsBar family _ is =
    B.listbar nextInletsBar
        [ Box.width $ Dimension.percents 90.0
        , Box.height $ Dimension.px 1
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        -- , List.items is


        , ListBar.commands $ mapWithIndex (\idx hiinr -> Node.withInputInNodeMRepr hiinr (inletHandler curPatchId curPatch nextNodeBox idx)) is


        -- , ListBar.commands $ List.toUnfoldable $ mapWithIndex inletHandler $ is
        , List.mouse true
        , List.keys true
        , ListBar.autoCommandKeys true
        , Style.inletsOutlets
        {- , Core.on ListBar.Select
            \_ _ -> do
                liftEffect $ Console.log "inlet"
                inletSelected <- List.selected ~< nextInletsBar
                liftEffect $ Console.log $ show inletSelected
        -}
        ]
        [ ]


{-
connectToOutput
    ::
        forall fA fB iB oA dinB doutA stateA stateB isA isB isB' osA osB osA'
        . IsSymbol fA
    => IsSymbol fB
    => Id.HasInput iB dinB isB' isB
    => FromRepr Hydra.BlessedRepr dinB
    => Id.HasOutput oA doutA osA' osA
    => ToRepr doutA Hydra.BlessedRepr
    => Proxy dinB
    -> Noodle.Node fB stateB isB osB Effect
    -> Id.Input iB
    -> Proxy doutA
    -> Noodle.Node fA stateA isA osA Effect
    -> Id.Output oA
    -> Effect (Noodle.Patch Hydra.State (Hydra.Instances Effect))
connectToOutput pdin inode inputId pdout onode outputId = do
    link <- Node.connectByRepr (Proxy :: _ Hydra.BlessedRepr) pdout pdin outputId inputId onode inode
    let nextPatch' = Patch.registerLink link curPatch
    pure nextPatch'
-}
inletHandler :: forall f nstate i din is is' os. IsSymbol f => Id.HasInput i din is' is => ToRepr din Hydra.BlessedRepr => FromRepr Hydra.BlessedRepr din => Patch.Id -> Patch Hydra.State (Hydra.Instances Effect) -> NodeBoxKey -> Int -> Proxy din -> Noodle.Node f nstate is os Effect -> Id.Input i -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-inlets-bar" State
inletHandler curPatchId curPatch nextNodeBox idx pdin inode inputId =
    Id.reflect inputId /\ [] /\ \_ _ -> do
        let inodeKey = nextNodeBox
        state <- State.get
        -- liftEffect $ Console.log $ "handler " <> iname
        case state.lastClickedOutlet of
            Just lco ->
                if inodeKey /= lco.nodeKey then do
                    linkCmp <- Link.create
                                state.lastLink
                                lco.nodeKey
                                (OutletIndex lco.index)
                                inodeKey
                                (InletIndex idx)
                    State.modify_ $ Link.store linkCmp
                    Key.patchBox >~ Link.append linkCmp
                    nextPatch' <- liftEffect $ Node.withOutputInNodeMRepr
                        (lco.outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.BlessedRepr) -- w/o type given here compiler fails to resolve constraints somehow
                        (\_ onode outputId -> do
                            link <- Node.connectByRepr (Proxy :: _ Hydra.BlessedRepr) outputId inputId onode inode
                            let nextPatch' = Patch.registerLink link curPatch
                            pure nextPatch'
                        )
                        -- (connectToOutput pdin inode inputId)
                        -- (\pdout node outputId -> ?wh)
                        -- (connectToOutput pdin inode inputId)

                    State.modify_ (\s -> s { network = wrapN $ Network.withPatch curPatchId (const nextPatch') $ unwrapN $ s.network })

                        {-
                        \pdout onode outputId -> do
                            -- link <- Node.connectByRepr (Proxy :: _ Hydra.BlessedRepr) outputId inputId onode inode
                            link <-
                                Node.connect outputId inputId
                                    ?wh
                                    -- (\dout -> ?wh (toRepr dout :: Maybe (Repr Hydra.BlessedRepr)))
                                    onode inode
                            let nextPatch' = Patch.registerLink link curPatch
                            pure nextPatch'
                            -- Patch.withNode lco.node
                            --     \patch onode ->
                                    --pure unit
                                    -- ?wh
                                    -- let toRepr
                                    -- link <- Node.connectByRepr (Proxy :: _ Hydra.BlessedRepr) outputId inputId onode node
                                    -- let nextPatch' = Patch.registerLink link curPatch
                                    -- pure nextPatch'
                                    -- pure unit

                                    -- Patch.connect outputId inputId identity onode node patch

                        -}

                    {-
                    _ <- Id.withNodeId lco.nodeId (\onodeId ->
                        case (/\) <$> Patch.findNode onodeId curPatch <*> Patch.findNode inodeId curPatch of
                            Just (onode /\ inode) ->
                        -- TODO: Patch.findNode
                                Id.withOutput
                                    lco.outputId
                                    (\outputId -> do
                                        _ <- Patch.connect outputId inputId ?wh onode inode curPatch
                                        pure unit
                                    )
                            Nothing -> pure unit
                    )
                    -}
                    -- TODO: Patch.connect
                    linkCmp # Link.on Element.Click onLinkClick
                else pure unit
            Nothing -> pure unit
        State.modify_
            (_ { lastClickedOutlet = Nothing })
    -- onInletSelect nodeId input nextNodeBox idx (Id.reflect input)


-- TODO: to Link module?
onLinkClick :: forall id. Link -> Line <^> id → EventJson → BlessedOp State Effect
onLinkClick link _ _ = do
    -- liftEffect $ Console.log "click link"
    Key.patchBox >~ Link.remove link
    State.modify_ $ Link.forget link
    Key.mainScreen >~ Screen.render