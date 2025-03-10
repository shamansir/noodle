module Cli.Components.Editor.Textual where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)

import Type.Data.Symbol (class IsSymbol)

import Control.Monad.Error.Class (class MonadThrow)

import Data.Tuple.Nested ((/\), type (/\))

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedOp (lift) as Blessed

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core as Core

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent)  as Box
-- import Blessed.UI.Base.Element.Option (index) as Element
import Blessed.UI.Base.Element.Method (focus, setFront, hide, show) as Element
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
-- import Blessed.UI.Boxes.Box.Method (focus) as Box
import Blessed.UI.Forms.TextArea.Option (inputOnFocus, mouse) as TextArea
import Blessed.UI.Forms.TextArea.Event (TextAreaEvent(..)) as TextArea
import Blessed.UI.Forms.TextArea.Property (value) as TextArea

import Cli.Style as Style

import Cli.Keys (mainScreen, patchBox, textValueEditor, TextValueEditorKey, ValueEditorKey) as Key
import Cli.Components.ValueEditor (ValueEditor)


tveKey :: Key.TextValueEditorKey
tveKey = Key.textValueEditor


editor :: forall state m. MonadThrow Error m => ValueEditor String state m
editor = fromKey tveKey


fromKey :: forall key state m. MonadThrow Error m => IsSymbol key => Key.ValueEditorKey key -> ValueEditor String state m
fromKey editorKey initialValue sendValue =
    { create : create editorKey initialValue sendValue
    , inject : \v -> editorKey >~ Box.setContent v
    , transpose : transpose editorKey
    }


create :: forall key state m. IsSymbol key => Key.ValueEditorKey key -> String -> (String -> Effect Unit) -> BlessedOp state m
create editorKey initialValue sendValue = do
    let
        innerTextBox =
            B.textBox editorKey
                [ Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.px 10
                , Box.height $ Dimension.px 1
                -- , Box.index 1
                , Style.chInputBox
                , TextArea.mouse true
                , Box.content initialValue
                , TextArea.inputOnFocus true
                , Core.on TextArea.Submit
                    \_ _ -> do
                        content <- TextArea.value ~< editorKey
                        Blessed.lift $ sendValue content
                        editorKey >~ Element.hide
                        Key.mainScreen >~ Screen.render
                ]
                [  ]
    --nodeBoxKey >~ Node.append innerText
    Key.patchBox >~ Node.append innerTextBox
    -- editorKey >~ Box.setContent initialValue
    editorKey >~ Element.setFront
    editorKey >~ Element.hide


transpose :: forall key state m. IsSymbol key => MonadThrow Error m => Key.ValueEditorKey key -> { x :: Int, y :: Int } -> BlessedOp state m
transpose editorKey { x, y } = do
    editorKey >~ Element.setTop  $ Offset.px $ y -- inodeBounds.top - 1
    editorKey >~ Element.setLeft $ Offset.px $ x -- inodeBounds.left
    editorKey >~ Element.setFront
    editorKey >~ Element.show
    editorKey >~ Element.focus
    -- Key.mainScreen >~ Screen.render


{-
editor' :: forall key state m. IsSymbol key => Key.ValueEditorKey key -> ValueEditor String state m
editor' editorKey curValue sendValue =
    editorKey /\ do
    let
        innerText =
            B.textBox editorKey
                [ Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , Box.width $ Dimension.px 10
                , Box.height $ Dimension.px 1
                -- , Box.index 1
                , Style.chInputBox
                , TextArea.mouse true
                , Box.content curValue
                , TextArea.inputOnFocus true
                , Core.on TextArea.Submit
                    \_ _ -> do
                        content <- TextArea.value ~< editorKey
                        Blessed.lift $ sendValue content
                        editorKey >~ Element.hide
                        Key.mainScreen >~ Screen.render
                ]
                [  ]
    --nodeBoxKey >~ Node.append innerText
    Key.patchBox >~ Node.append innerText
    editorKey >~ Element.setFront
    editorKey >~ Element.hide
    -- neTextBoxKey >~ Element.focus
    -- pure textBoxKey
-}




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit