module Cli.Components.SidePanel.Console where

import Prelude


import Effect (Effect)
import Effect.Class (class MonadEffect)

import Control.Monad.State (modify_) as State

import Data.Tuple.Nested ((/\))
import Data.Array (singleton) as Array
import Data.Text.Format as T

import Blessed.Internal.BlessedOp (BlessedOp)

import Cli.State (State)
import Cli.State (togglePanel, isPanelOn, appendToLog, clearLog) as CState
import Cli.Components.SidePanel (SidePanel)
import Cli.Components.SidePanel (refresh) as SP
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Front.Shared.Panels (Which(..)) as P


sidePanel :: forall tk p fs sr cr m. SidePanel "console" (State _ tk p fs sr cr m) Boolean
sidePanel =
    { title : "console"
    , char : const 'L'
    , isOn : identity
    , panelKey : Key.consoleBox
    , buttonKey : Key.consoleButton
    , next : (\s -> pure $ CState.isPanelOn P.Console s /\ (T.s <$> s.developmentLog))
    , onToggle : CState.togglePanel P.Console
    }


log :: forall tk p fs sr cr m mi. MonadEffect m => String -> BlessedOp (State _ tk p fs sr cr mi) m
log line = do
    State.modify_ $ CState.appendToLog line
    SP.refresh sidePanel


logError :: forall tk p fs sr cr m mi. MonadEffect m => String -> BlessedOp (State _ tk p fs sr cr mi) m
logError errorStr = log $ "Error : " <> errorStr


clear :: forall tk p fs sr cr m mi. MonadEffect m => BlessedOp (State _ tk p fs sr cr mi) m
clear = do
    State.modify_ CState.clearLog
    SP.refresh sidePanel