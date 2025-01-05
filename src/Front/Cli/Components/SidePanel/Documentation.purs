module Cli.Components.SidePanel.Documentation where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\))
import Data.Text.Format as T

import Control.Monad.State (modify_) as State

import Cli.State (State)
import Cli.State (togglePanel, isPanelOn, switchDocumentation, clearDocumentation) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel (refresh) as SP
import Cli.Keys as Key
import Cli.Panels (Which(..)) as P

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Id (FamilyR) as Id


sidePanel :: forall tk p fs sr cr m. SidePanel "documentation" (State tk p fs sr cr m) Boolean
sidePanel =
    { title : "documentation"
    , char : const '☰'
    , isOn : identity
    , panelKey : Key.documentationBox
    , buttonKey : Key.documentationButton
    , next : \s -> CState.isPanelOn P.Documentation s /\ (T.s <$> s.currentDocumentation)
    , onToggle : CState.togglePanel P.Documentation
    }


showDocumentationFor :: forall tk p fs sr cr m mi. MonadEffect m => Id.FamilyR -> BlessedOp (State tk p fs sr cr mi) m
showDocumentationFor familyR = do
    State.modify_ $ CState.switchDocumentation familyR
    SP.refresh sidePanel


clear :: forall tk p fs sr cr m mi. MonadEffect m => BlessedOp (State tk p fs sr cr mi) m
clear = do
    State.modify_ CState.clearDocumentation
    SP.refresh sidePanel