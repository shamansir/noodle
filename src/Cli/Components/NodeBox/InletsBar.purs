module Cli.Components.NodeBox.InletsBar where

import Prelude

import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))

import Blessed ((>~))
import Blessed as B

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Key (Key) as C
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedSubj (Line, ListBar)

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option (keys, mouse) as List
import Blessed.UI.Lists.ListBar.Option (autoCommandKeys, commands) as ListBar
import Blessed.Internal.Core as Core

import Cli.Keys (NodeBoxKey, InletsBarKey)
import Cli.Keys as Key
import Cli.Style as Style
import Cli.State (State, Link, OutletIndex(..), InletIndex(..))
import Cli.State.NwWraper (wrapN, unwrapN)
import Cli.Components.Link as Link

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Network2 as Network
import Noodle.Family.Def as Family

import Toolkit.Hydra2 (Instances, State) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


component
    :: forall f state is os
    -- :: forall id r f state fs iis rli is rlo os repr_is repr_os
    --  . Hydra.HasNodesOf f state fs iis rli is rlo os Effect
    -- => R.ToReprHelper Effect f is rli os rlo repr_is repr_os Hydra.WrapRepr state
    -- => FromToReprRow rli is Hydra.WrapRepr
    -- => FromToReprRow rlo os Hydra.WrapRepr
    -- => Node.NodeBoundKeys Node.I rli Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -- => Node.NodeBoundKeys Node.O rlo Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
     . Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> InletsBarKey
    -> Id.Family f
    -> Family.Def state is os Effect
    -> Array (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    -> C.Blessed State
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


inletHandler
    :: forall f nstate i din is is' os
     . IsSymbol f
    => Id.HasInput i din is' is
    => ToRepr din Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr din
    => Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> Int
    -> Proxy din
    -> Noodle.Node f nstate is os Effect
    -> Id.Input i
    -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-inlets-bar" State
inletHandler curPatchId curPatch nextNodeBox idx _ inode inputId =
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
                        (lco.outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr) -- w/o type given here compiler fails to resolve constraints somehow
                        (\_ onode outputId -> do
                            link <- Node.connectByRepr (Proxy :: _ Hydra.WrapRepr) outputId inputId onode inode
                            let nextPatch' = Patch.registerLink link curPatch
                            pure nextPatch'
                        )

                    State.modify_ (\s -> s { network = wrapN $ Network.withPatch curPatchId (const nextPatch') $ unwrapN $ s.network })

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