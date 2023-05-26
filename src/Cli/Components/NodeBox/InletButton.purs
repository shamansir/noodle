module Cli.Components.NodeBox.InletButton where

import Prelude


import Effect.Class (liftEffect)

import Effect.Console (log) as Console

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.Core as Core

import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.Palette as Palette
import Cli.State (State)
import Cli.State (patchIdFromIndex) as State
import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Components.PatchesBar as PatchesBar
import Cli.Style as Style

import Noodle.Network2 as Network
import Noodle.Patch4 as Patch

import Toolkit.Hydra2 as Hydra

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

import Cli.Keys (NodeBoxKey, InletsBoxKey)
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
    -> Core.Blessed State
component curPatchId curPatch nextNodeBox idx pdin inode inputId =
    B.button Key.inletButton
        [ Box.content "+"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px $ idx * 2
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> onPress curPatchId curPatch nextNodeBox idx pdin inode inputId
        , Core.on Element.MouseOver
            \_ _ -> onMouseOver
        , Core.on Element.MouseOut
            \_ _ -> onMouseOut
        ]
        []


onMouseOver :: BlessedOp State Effect
onMouseOver =
    liftEffect $ Console.log "over"


onMouseOut :: BlessedOp State Effect
onMouseOut =
    liftEffect $ Console.log "out"


onPress
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
    -> BlessedOp State Effect
onPress curPatchId curPatch nextNodeBox idx _ inode inputId =
    {-Id.reflect inputId /\ [] /\ \_ _ -> -} do
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


-- TODO: move to Link module?
onLinkClick :: forall id. Link -> Line <^> id → EventJson → BlessedOp State Effect
onLinkClick link _ _ = do
    -- liftEffect $ Console.log "click link"
    Key.patchBox >~ Link.remove link
    State.modify_ $ Link.forget link
    Key.mainScreen >~ Screen.render