module Cli.Components.NodeBox.InletButton where

import Prelude


import Effect (Effect)
import Effect.Class (liftEffect)

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Mark (mark)
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.SProxy (reflect)

import Type.Proxy (Proxy(..))
import Signal (Signal)
import Signal (get) as Signal

import Blessed as B
import Blessed ((>~))
import Blessed.Tagger as T
import Blessed.Tagger ((<:>))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedSubj (Line)
import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method as Box

import Cli.Keys as Key
import Cli.Keys (NodeBoxKey, InfoBoxKey, InletButtonKey, mainScreen, statusLine)
import Cli.State (State, Link, OutletIndex(..), InletIndex(..))
import Cli.State.NwWraper (unwrapN, wrapN)
import Cli.Style (inletsOutlets) as Style
import Cli.Components.Link as Link
import Cli.Palette.Set.X11 as X11
import Cli.Palette.Item (crepr) as C
import Cli.Palette as Palette

import Noodle.Network2 as Network
import Noodle.Patch4 as Patch

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 (Patch)

import Toolkit.Hydra2 (Instances, State) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Repr.Info (short, full) as Info


width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


content :: forall i. IsSymbol i => Int -> Id.Input i -> Maybe Hydra.WrapRepr -> String
content idx inputId = content' idx $ Id.inputR inputId

-- TODO: mark with color
content' :: Int -> Id.InputR -> Maybe Hydra.WrapRepr -> String
content' idx inputId (Just repr) =
    T.render $ T.fgcs (mark repr) $ Info.short repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
content' idx inputId Nothing = "⋱" <> show idx <> "⋰"


slContent :: forall f i. IsSymbol i => IsSymbol f => Id.Family' f -> Int -> Id.Input i -> Maybe Hydra.WrapRepr -> String
slContent family idx inputId = slContent' (Id.familyR' family) idx $ Id.inputR inputId


slContent' :: Id.FamilyR -> Int -> Id.InputR -> Maybe Hydra.WrapRepr -> String
slContent' familyR idx inputId (Just repr) =
    -- TODO: show group as well
    T.render $ (T.fgcs (C.crepr Palette.familyName) $ Id.reflectFamilyR familyR) <:> T.s " " <:> (T.fgcs (C.crepr Palette.inputId) $ Id.reflectInputR inputId) <:> T.s " " <:> (T.fgcs (mark repr) $ Info.full repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
slContent' familyR idx inputId Nothing = "⋱" <> show idx <> "⋰"


component
    :: forall f nstate i din is is' os
     . IsSymbol f
    => Id.HasInput i din is' is
    => ToRepr din Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr din
    => InletButtonKey
    -> InfoBoxKey
    -> Patch.Id
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> NodeBoxKey
    -> Int
    -> Maybe Hydra.WrapRepr
    -> Signal (Maybe Hydra.WrapRepr)
    -> Proxy din -- Hydra.WrapRepr?
    -> Noodle.Node f nstate is os Effect
    -> Id.Input i
    -> Core.Blessed State
component buttonKey nextInfoBox curPatchId curPatch nextNodeBox idx maybeRepr reprSignal pdin inode inputId =
    B.button buttonKey
        [ Box.content $ content idx inputId maybeRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            $ onPress curPatchId curPatch nextNodeBox idx pdin inode inputId
        , Core.on Element.MouseOver
            $ onMouseOver (Node.family inode) nextInfoBox idx inputId maybeRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut nextInfoBox idx
        ]
        []


onMouseOver :: forall i f. IsSymbol i => IsSymbol f => Id.Family' f ->  InfoBoxKey -> Int -> Id.Input i -> Maybe Hydra.WrapRepr -> Signal (Maybe Hydra.WrapRepr) -> _ -> _ -> BlessedOp State Effect
onMouseOver family infoBox idx inputId _ reprSignal _ _ = do
    maybeRepr <- liftEffect $ Signal.get reprSignal
    -- infoBox >~ Box.setContent $ show idx <> " " <> reflect inputId
    infoBox >~ Box.setContent $ T.render $ T.fgcs (C.crepr Palette.inputId) $ reflect inputId
    statusLine >~ Box.setContent $ slContent family idx inputId maybeRepr
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: InfoBoxKey -> Int ->  _ -> _ -> BlessedOp State Effect
onMouseOut infoBox idx _ _ = do
    infoBox >~ Box.setContent ""
    statusLine >~ Box.setContent ""
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "out" <> show idx


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
    -> _
    -> _
    -> BlessedOp State Effect
onPress curPatchId curPatch nextNodeBox idx _ inode inputId _ _ =
    {-Id.reflect inputId /\ [] /\ \_ _ -> -} do
        let altIdx = Id.index inputId
        -- liftEffect $ Console.log $ "press" <> show idx
        -- liftEffect $ Console.log $ "apress" <> show altIdx
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
        Key.mainScreen >~ Screen.render -- FIXME: only re-render patchBox
    -- onInletSelect nodeId input nextNodeBox idx (Id.reflect input)


-- TODO: move to Link module?
onLinkClick :: forall id. Link -> Line <^> id → EventJson → BlessedOp State Effect
onLinkClick link _ _ = do
    -- liftEffect $ Console.log "click link"
    Key.patchBox >~ Link.remove link
    State.modify_ $ Link.forget link
    Key.mainScreen >~ Screen.render