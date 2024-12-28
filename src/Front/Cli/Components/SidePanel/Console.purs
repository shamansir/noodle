module Cli.Components.SidePanel.Console where

import Prelude


import Effect (Effect)
import Effect.Class (class MonadEffect)

import Control.Monad.State (modify_) as State

import Data.Tuple.Nested ((/\))
import Data.Array (singleton) as Array

import Blessed.Internal.BlessedOp (BlessedOp)

import Cli.State (State, withPanels)
import Cli.Components.SidePanel (SidePanel)
import Cli.Components.SidePanel (refresh) as SP
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..), load, toggle, logToConsole)


sidePanel :: forall tk p fs sr cr m. SidePanel "console" (State tk p fs sr cr m) Boolean
sidePanel =
    { title : "console"
    , char : const 'L'
    , isOn : identity
    , panelKey : Key.consoleBox
    , buttonKey : Key.consoleButton
    , init : false /\ []
    , next : _.panels >>> load Console
    , onToggle : withPanels $ toggle Console
    }


log :: forall tk p fs sr cr m mi. MonadEffect m => String -> BlessedOp (State tk p fs sr cr mi) m
log s = do
    State.modify_ (withPanels $ logToConsole $ Array.singleton s)
    SP.refresh sidePanel
