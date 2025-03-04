module Cli.Components.SidePanel.CommandLog where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\))

import Control.Monad.State (modify_) as State

import Cli.State (State)
import Cli.State (formatHistory, trackCommandOp, clearHistory, togglePanel, isPanelOn) as CState
import Cli.Components.SidePanel (SidePanel)
import Cli.Components.SidePanel (refresh) as SP
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..)) as P

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Text.NdfFile.Command.Op (CommandOp)


sidePanel :: forall tk p fs sr cr m. SidePanel "command-log" (State tk p fs sr cr m) Boolean
sidePanel =
    { title : "history"
    , char : const '⏺'
    , isOn : identity
    , panelKey  : Key.commandLogBox
    , buttonKey : Key.commandLogButton
    , next : (\s -> pure $ CState.isPanelOn P.Commands s /\ CState.formatHistory s)
    , onToggle : CState.togglePanel P.Commands
    }


trackCommand :: forall tk p fs sr cr m mi. MonadEffect m => CommandOp -> BlessedOp (State tk p fs sr cr mi) m
trackCommand cmdOp = do
    State.modify_ $ CState.trackCommandOp cmdOp
    SP.refresh sidePanel


clear :: forall tk p fs sr cr m mi. MonadEffect m => BlessedOp (State tk p fs sr cr mi) m
clear = do
    State.modify_ $ CState.clearHistory
    SP.refresh sidePanel