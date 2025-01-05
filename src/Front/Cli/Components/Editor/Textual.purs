module Cli.Components.Editor.Textual where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedOp (lift) as Blessed

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core as Core

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Boxes.Box.Option as Box
-- import Blessed.UI.Base.Element.Option (index) as Element
import Blessed.UI.Base.Element.Method (focus, setFront, hide) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
-- import Blessed.UI.Boxes.Box.Method (focus) as Box
import Blessed.UI.Forms.TextArea.Option (inputOnFocus, mouse) as TextArea
import Blessed.UI.Forms.TextArea.Event (TextAreaEvent(..)) as TextArea
import Blessed.UI.Forms.TextArea.Property (value) as TextArea

import Cli.Style as Style

import Cli.Keys (mainScreen, patchBox, textValueEditor, TextValueEditorKey) as Key
import Cli.Components.ValueEditor (ValueEditor)


tveKey :: Key.TextValueEditorKey
tveKey = Key.textValueEditor


-- render :: forall m. NodeBoxKey -> Node m -> BlessedOp FNumber.State m
-- editor :: forall f m i din is' is state os
--      . MonadEffect m => NodeBoxKey -> Number -> (Number -> Effect Unit) -> BlessedOp Number m
editor :: forall state m. ValueEditor String state m
-- render :: NodeBoxKey -> Node Effect -> BlessedOp FNumber.State Effect
editor curValue sendValue =
    NK.toRaw tveKey /\ do
    let
        --(rootTextBoxKey :: NETextBoxKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        -- neTextBoxKey = NK.append patchBoxKey rootTextBoxKey
        innerText =
            B.textBox tveKey
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
                        content <- TextArea.value ~< tveKey
                        Blessed.lift $ sendValue content
                        tveKey >~ Element.hide
                        Key.mainScreen >~ Screen.render
                ]
                [  ]
    --nodeBoxKey >~ Node.append innerText
    Key.patchBox >~ Node.append innerText
    tveKey >~ Element.setFront
    tveKey >~ Element.hide
    -- neTextBoxKey >~ Element.focus
    -- pure textBoxKey





-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit