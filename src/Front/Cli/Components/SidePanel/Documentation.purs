module Cli.Components.SidePanel.Documentation where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.Text.Format as T

import Control.Monad.State (modify_) as State

import Cli.State (State)
import Cli.State (withPanels) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel (refresh) as SP
import Cli.Keys as Key
import Cli.Panels (Which(..), load, toggle)

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Id (FamilyR) as Id
import Noodle.Text.NdfFile (documentationFor) as Ndf


sidePanel :: forall tk p fs sr cr m. SidePanel "documentation" (State tk p fs sr cr m) Boolean
sidePanel =
    { title : "documentation"
    , char : const '☰'
    , isOn : identity
    , panelKey : Key.documentationBox
    , buttonKey : Key.documentationButton
    , init : false /\ []
    , next : _.panels >>> load Documentation
    , onToggle : CState.withPanels $ toggle Documentation
    }


showDocumentationFor :: forall tk p fs sr cr m mi. MonadEffect m => Id.FamilyR -> BlessedOp (State tk p fs sr cr mi) m
showDocumentationFor familyR = do
    State.modify_ $ CState.withPanels $
        \s -> s { documentation =
                s.documentation $>
                    (T.s <$> (Ndf.documentationFor familyR $ Tuple.snd $ s.commands))
                }
    SP.refresh sidePanel