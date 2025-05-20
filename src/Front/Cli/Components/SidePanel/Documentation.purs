module Cli.Components.SidePanel.Documentation where

import Prelude

import Effect.Class (class MonadEffect)

import Type.Proxy (Proxy(..))

import Data.Tuple.Nested ((/\))
import Data.Text.Format as T
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Array (length) as Array
import Data.Array ((:))

import Control.Monad.State (modify_) as State

import Cli.State (State, DocumentationFocus)
import Cli.State (togglePanel, isPanelOn, switchDocumentation, clearDocumentation) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel (refresh) as SP
import Cli.Keys as Key
import Front.Shared.Panels (Which(..)) as P

import Noodle.Ui.Tagging as Tagging
import Noodle.Ui.Tagging.At (class At, at) as Tagged
import Noodle.Ui.Tagging.At (Documentation, documentation) as At

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
    => SidePanel "documentation" (State _ tk p fs sr cr m) Boolean
sidePanel =
    { title : "documentation"
    , char : const '☰'
    , isOn : identity
    , panelKey : Key.documentationBox
    , buttonKey : Key.documentationButton
    , next : \s -> pure $
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
    [ Tagging.comment "Defaults:"
    , familyDocs
    , Tagging.comment "Current state:"
    , fromMaybe T.nil mdUpdateLines
    ]
    <>
    (if hasDocumentation then
        Tagging.comment "Documentation:" : (T.s <$> documentationLines)
    else [])
    where
        familyDocs = Tagging.familyDocs ptk (Id.familyOf node)
        documentationLines = Ndf.documentationFor (Id.familyOf node) ndfFile
        mdUpdateLines = Tagging.nodeDocumentation ptk node <$> curUpdate
        hasDocumentation = Array.length documentationLines > 0

 -- (T.s <$> s.currentDocumentation)
 -- s { currentDocumentation = s.history # Ndf.documentationFor familyR }


showFamilyDocumentation
    :: forall tk p fs sr cr m mi
     . MonadEffect m
    => MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Id.FamilyR -> BlessedOp (State _ tk p fs sr cr mi) m
showFamilyDocumentation familyR = do
    -- State.modify_ $ CState.switchDocumentation ?wh Nothing
    SP.refresh sidePanel


showNodeDocumentation
    :: forall tk p fs sr cr m mi
     . MonadEffect m
    => MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Id.NodeR -> Maybe (Raw.NodeChanges sr cr) -> BlessedOp (State _ tk p fs sr cr mi) m
showNodeDocumentation nodeR mbUpdate = do
    State.modify_ $ CState.switchDocumentation nodeR mbUpdate
    SP.refresh sidePanel


clear
    :: forall tk p fs sr cr m mi
     . MonadEffect m
    => MarkToolkit tk
    => HasChRepr tk cr
    => Tagged.At At.Documentation cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => BlessedOp (State _ tk p fs sr cr mi) m
clear = do
    State.modify_ CState.clearDocumentation
    SP.refresh sidePanel