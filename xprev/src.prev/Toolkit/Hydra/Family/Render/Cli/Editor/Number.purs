module Toolkit.Hydra.Family.Render.Cli.Editor.Number where

import Prelude


import Prelude

import Type.Proxy (Proxy)
import Data.Number as Number
import Data.Repr (wrap)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Control.Monad.State (State)
import Control.Monad.State as State

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map

import Signal (Signal)

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (TextBox)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.BlessedOp (lift, impair) as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.Core.Border as Border
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Coord as C
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core as Core

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Boxes.Box.Option as Box
-- import Blessed.UI.Base.Element.Option (index) as Element
import Blessed.UI.Base.Element.Method (focus, setFront, hide) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
-- import Blessed.UI.Boxes.Box.Method (focus) as Box
import Blessed.UI.Forms.TextArea.Option as TextArea
import Blessed.UI.Forms.TextArea.Event as TextArea
import Blessed.UI.Forms.TextArea.Method as TextArea
import Blessed.UI.Forms.TextArea.Property as TextArea

import Noodle.Node (sendOut) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style

-- import Noodle.Node (Node)

import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)
-- import Toolkit.Hydra.Family.Feed.FNumber (Inputs, Outputs, Node)
-- import Toolkit.Hydra.Family.Feed.FNumber (Node, State, _out_out) as FNumber
-- import Toolkit.Hydra (State) as Hydra

import Toolkit.Hydra.Repr.Wrap (WrapRepr(..)) as H
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Id as Id
import Data.Symbol (class IsSymbol, reflectSymbol)

-- import Cli.State (State)
import Cli.Keys (PatchBoxKey)
import Cli.Keys (mainScreen, patchBox, numValueEditor, NumValueEditorKey) as Key

import Toolkit.Hydra.Family.Render.Editor (EditorId(..), HasEditors)

import Data.Repr (Repr, class FromRepr, class ToRepr, class DataFromToReprRow, toRepr, fromRepr, class ReadWriteRepr)


nveKey :: Key.NumValueEditorKey
nveKey = Key.numValueEditor



-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
-- editor :: forall f m i din is' is state os
--      . MonadEffect m => NodeBoxKey -> Number -> (Number -> Effect Unit) -> BlessedOp Number m
editor :: forall state m r. MonadEffect m => Number -> (H.WrapRepr -> Effect Unit) -> NK.RawNodeKey /\ BlessedOp (HasEditors r) m
-- render :: NodeBoxKey -> Node Effect -> BlessedOp FNumber.State Effect
editor curValue _ =
    NK.rawify nveKey /\ do
    let
        --(rootTextBoxKey :: NETextBoxKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        -- neTextBoxKey = NK.append patchBoxKey rootTextBoxKey
        innerText =
            B.textBox nveKey
                [ Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.px 10
                , Box.height $ Dimension.px 1
                -- , Box.index 1
                , Style.chInputBox
                , TextArea.mouse true
                , TextArea.inputOnFocus true
                , Core.on TextArea.Submit
                    \_ _ -> do
                        content <- TextArea.value ~< nveKey
                        let mbNumber = Number.fromString content
                        state <- State.get
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case (/\) <$> mbNumber <*> (Map.lookup (EditorId "number") state.editors >>= identity) of
                            Just (number /\ sendValue) ->
                                -- _ <- Node.withInputInNodeMRepr holdsInput (sendToInput $ H.Value $ H.Number number)
                                            -- Node.sendIn node input $ H.Value $ H.Number number
                                sendValue $ H.Value $ H.Number number --Node.sendIn input node 20.0-- $ T.Number number
                                -- Just number -> Node.sendOut node FNumber._out_out $ T.Number number
                            Nothing -> pure unit
                        nveKey >~ Element.hide
                        Key.mainScreen >~ Screen.render
                ]
                [  ]
    --nodeBoxKey >~ Node.append innerText
    Key.patchBox >~ Node.append innerText
    nveKey >~ Element.setFront
    nveKey >~ Element.hide
    -- neTextBoxKey >~ Element.focus
    -- pure textBoxKey





-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit