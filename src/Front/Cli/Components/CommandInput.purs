module Cli.Components.CommandInput where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)

import Type.Data.Symbol (class IsSymbol)

import Control.Monad.State (modify_, get) as State
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
-- import Blessed.UI.Base.Element.Option (index) as Element
import Blessed.UI.Boxes.Box.Method (setContent)  as Box
import Blessed.UI.Forms.TextArea.Method (setValue)  as TextArea
import Blessed.UI.Base.Element.Method (focus, setFront, hide, show) as Element
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
-- import Blessed.UI.Boxes.Box.Method (focus) as Box
import Blessed.UI.Forms.TextArea.Option (inputOnFocus, mouse) as TextArea
import Blessed.UI.Forms.TextArea.Event (TextAreaEvent(..)) as TextArea
import Blessed.UI.Forms.TextArea.Property (value) as TextArea

import Cli.State (State)

import Cli.Style as Style

import Cli.Keys (mainScreen, patchBox, commandInput, CommandInputKey) as Key


commandInputKey :: Key.CommandInputKey
commandInputKey = Key.commandInput


component :: forall tk ps fs sr cr m. Core.Blessed (State tk ps fs sr cr m)
component =
    B.textBoxAnd commandInputKey
        [ Box.top $ Offset.center
        , Box.left $ Offset.center
        , Box.width $ Dimension.px 60
        , Box.height $ Dimension.px 1
        -- , Box.index 1
        , Style.chInputBox
        , TextArea.mouse true
        , Box.content ""
        , TextArea.inputOnFocus true
        , Core.on TextArea.Submit
            \_ _ -> do
                content <- TextArea.value ~< commandInputKey
                -- liftEffect $ Console.log content
                -- Blessed.lift $ sendValue content
                -- commandInputKey >~ Element.setFront
                hide
        ]
        [  ]
    $ const hide

hide :: forall tk ps fs sr cr mi mo. BlessedOp (State tk ps fs sr cr mi) mo
hide = do
    State.modify_ $ _ { commandBoxActive = false }
    commandInputKey >~ Element.hide
    Key.mainScreen >~ Screen.render


show :: forall tk ps fs sr cr mi mo. BlessedOp (State tk ps fs sr cr mi) mo
show = do
    commandInputKey >~ Element.setFront
    commandInputKey >~ Element.show
    commandInputKey >~ Element.focus
    -- commandInputKey >~ Box.setContent ""
    commandInputKey >~ TextArea.setValue ""
    State.modify_ $ _ { commandBoxActive = true }
    Key.mainScreen >~ Screen.render


toggle :: forall tk ps fs sr cr mi mo. BlessedOp (State tk ps fs sr cr mi) mo
toggle =
    State.get >>= \s ->
        if s.commandBoxActive then
            hide
        else
            show