module Cli.Components.SidePanel.Documentation where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\))
import Data.Text.Format as T
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Type.Proxy (Proxy(..))

import Control.Monad.State (modify_) as State

import Cli.State (State, DocumentationFocus)
import Cli.State (togglePanel, isPanelOn, switchDocumentation, clearDocumentation) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel (refresh) as SP
import Cli.Keys as Key
import Cli.Panels (Which(..)) as P

import Noodle.Ui.Cli.Tagging as Tagging
import Noodle.Ui.Cli.Tagging.At (class At, at) as Tagged
import Noodle.Ui.Cli.Tagging.At (Documentation, documentation) as At

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Id (FamilyR, NodeR, familyOf) as Id
import Noodle.Raw.Node (NodeChanges) as Raw
import Noodle.Toolkit (class HasChRepr, class MarkToolkit)
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (documentationFor) as Ndf
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Repr.ValueInChannel (ValueInChannel)


sidePanel
    :: forall tk p fs sr cr m
     . MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => SidePanel "documentation" (State tk p fs sr cr m) Boolean
sidePanel =
    { title : "documentation"
    , char : const '☰'
    , isOn : identity
    , panelKey : Key.documentationBox
    , buttonKey : Key.documentationButton
    , next : \s ->
        CState.isPanelOn P.Documentation s
        /\ maybe []
            (loadDocumentation (Proxy :: _ tk) s.history)
            s.currentDocumentation
    , onToggle : CState.togglePanel P.Documentation
    }


loadDocumentation
    :: forall tk sr cr
     . MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Proxy tk -> NdfFile -> DocumentationFocus sr cr -> Array T.Tag
loadDocumentation ptk ndfFile { node, curUpdate } =
    (T.s <$> Ndf.documentationFor (Id.familyOf node) ndfFile)
    <>
    [ T.nl, Tagging.familyDocs ptk (Id.familyOf node)
    , fromMaybe T.nil $ Tagging.nodeDocumentation ptk node <$> curUpdate
    ]

 -- (T.s <$> s.currentDocumentation)
 -- s { currentDocumentation = s.history # Ndf.documentationFor familyR }


showFamilyDocumentation
    :: forall tk p fs sr cr m mi
     . MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Id.FamilyR -> BlessedOp (State tk p fs sr cr mi) m
showFamilyDocumentation familyR = do
    -- State.modify_ $ CState.switchDocumentation ?wh Nothing
    SP.refresh sidePanel


showNodeDocumentation
    :: forall tk p fs sr cr m mi
     . MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Id.NodeR -> Maybe (Raw.NodeChanges sr cr) -> BlessedOp (State tk p fs sr cr mi) m
showNodeDocumentation nodeR mbUpdate = do
    State.modify_ $ CState.switchDocumentation nodeR mbUpdate
    SP.refresh sidePanel


clear
    :: forall tk p fs sr cr m mi
     . MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => BlessedOp (State tk p fs sr cr mi) m
clear = do
    State.modify_ CState.clearDocumentation
    SP.refresh sidePanel