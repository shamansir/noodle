module Cli.Components.SidePanel.CommandLog where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\))
import Data.Text.Format as F
import Data.Array as Array

import Control.Monad.State (modify_) as State

import Cli.State (State)
import Cli.State (withPanels) as State
import Cli.Components.SidePanel (SidePanel)
import Cli.Components.SidePanel (refresh) as SP
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Cli.Panels (Which(..)) as P
import Cli.Panels (load, toggle, initCommands, appendCommand) as Panels

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Text.NdfFile (toTaggedNdfCode)
import Noodle.Text.NdfFile.Command.Op (CommandOp)

sidePanel :: forall tk p fs sr cr m. SidePanel "command-log" (State tk p fs sr cr m) Boolean
sidePanel =
    { title : "history"
    , char : const '⏺'
    , isOn : identity
    , panelKey : Key.commandLogBox
    , buttonKey : Key.commandLogButton
    , init : false /\ (Array.singleton $ toTaggedNdfCode Panels.initCommands)
    , next : _.panels >>> Panels.load P.Commands
    , onToggle : State.withPanels $ Panels.toggle P.Commands
    }


trackCommand :: forall tk p fs sr cr m mi. MonadEffect m => CommandOp -> BlessedOp (State tk p fs sr cr mi) m
trackCommand cmdOp = do
    State.modify_ $ State.withPanels $ Panels.appendCommand cmdOp
    SP.refresh sidePanel