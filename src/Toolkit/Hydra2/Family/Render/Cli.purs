module Toolkit.Hydra2.Family.Render.Cli where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody, class HasBody', class HasCustomSize) {-, class HasEditor, class HasEditor', class HasEditor'')-}

import Cli.Keys (NodeBoxKey)

import Blessed.Internal.Core (Blessed)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (RawNodeKey, NodeKey, type (<^>))

import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, State, Node) as FNumber
import Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber (render) as FNumber
import Toolkit.Hydra2.Family.Out.FOut (Inputs, Outputs, State, Node) as FOut
import Toolkit.Hydra2.Family.Render.Cli.Out.FOut (render) as FOut
import Toolkit.Hydra2.Family.Display.FInfo (Inputs, Outputs, State, Node) as FInfo
import Toolkit.Hydra2.Family.Render.Cli.Display.FInfo (render) as FInfo
import Toolkit.Hydra2.Family.Feed.FCallback (Inputs, Outputs, State, Node) as FCallback
import Toolkit.Hydra2.Family.Render.Cli.Feed.FCallback (render) as FCallback
import Toolkit.Hydra2.Family.Feed.FArray (Inputs, Outputs, State, Node) as FArray
import Toolkit.Hydra2.Family.Render.Cli.Feed.FArray (render) as FArray

import Toolkit.Hydra2.Family.Render.Cli.Editor.Number as ENumber

import Toolkit.Hydra2.Repr.Wrap (WrapRepr)
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..)) as H
import Toolkit.Hydra2.Types as H

import Noodle.Node2 (Node)
import Noodle.Id as Id

import Toolkit.Hydra2.Family.Render.Editor (EditorId(..), HasEditors)


data CliF (f :: Symbol) = CliF


data CliD din = CliD


instance (Applicative m, MonadEffect m) => HasBody (CliF "number") "number" FNumber.State FNumber.Inputs FNumber.Outputs m where
    run :: Proxy (CliF "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    run _ = FNumber.render
else instance (MonadRec m, MonadEffect m) => HasBody (CliF "out") "out" FOut.State FOut.Inputs FOut.Outputs m where
    run :: Proxy (CliF "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
    run _ = FOut.render
else instance (MonadRec m, MonadEffect m) => HasBody (CliF "info") "info" FInfo.State FInfo.Inputs FInfo.Outputs m where
    run :: Proxy (CliF "info") -> NodeBoxKey -> FInfo.Node m -> BlessedOp FInfo.State m
    run _ = FInfo.render
else instance (Applicative m, MonadEffect m) => HasBody (CliF "callback") "callback" FCallback.State FCallback.Inputs FCallback.Outputs m where
    run :: Proxy (CliF "callback") -> NodeBoxKey -> FCallback.Node m -> BlessedOp FCallback.State m
    run _ = FCallback.render
else instance (Applicative m, MonadEffect m) => HasBody (CliF "array") "array" FArray.State FArray.Inputs FArray.Outputs m where
    run :: Proxy (CliF "array") -> NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
    run _ = FArray.render
else instance HasBody (CliF f) f state is os m where
    run :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
    run _ _ _ = pure unit


instance (Applicative m, MonadEffect m) => HasBody' (CliF "number") (FNumber.Node m) FNumber.State m where
    run' :: Proxy (CliF "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    run' _ = FNumber.render
else instance (MonadRec m, MonadEffect m) => HasBody' (CliF "out") (FOut.Node m) FOut.State m where
    run' :: Proxy (CliF "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
    run' _ = FOut.render
else instance (MonadRec m, MonadEffect m) => HasBody' (CliF "info") (FInfo.Node m) FInfo.State m  where
    run' :: Proxy (CliF "info") -> NodeBoxKey -> FInfo.Node m -> BlessedOp FInfo.State m
    run' _ = FInfo.render
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "callback") (FCallback.Node m) FCallback.State m where
    run' :: Proxy (CliF "callback") -> NodeBoxKey -> FCallback.Node m -> BlessedOp FCallback.State m
    run' _ = FCallback.render
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "array") (FArray.Node m) FArray.State m where
    run' :: Proxy (CliF "array") -> NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
    run' _ = FArray.render
else instance HasBody' (CliF f) (Node f state is os m) state m where
    run' :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
    run' _ _ _ = pure unit


instance HasCustomSize (CliF "callback") (FCallback.Node m) where
    size :: Proxy (CliF "callback") -> NodeBoxKey -> FCallback.Node m -> Maybe { width :: Int, height :: Int }
    size _ _ _ = Just { width : 15, height : 10 }
else instance HasCustomSize (CliF f) (Node f state is os m) where
    size :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> Maybe { width :: Int, height :: Int }
    size _ _ _ = Nothing


{-
instance Id.HasInput i Number is' is => HasEditor (CliD Number) is' (Id.Input i) (Node f nstate is os m) Number m where
    editor :: Proxy is' -> Proxy (CliD Number) -> NodeBoxKey -> Id.Input i -> Node f nstate is os m -> Maybe (BlessedOp Number m)
    editor _ _ key input node = Just $ ENumber.editor key node input
else instance HasEditor (CliD din) is' (Id.Input i) (Node f nstate is os m) din m where
    editor :: Proxy is' -> Proxy (CliD din) -> NodeBoxKey -> Id.Input i -> Node f nstate is os m -> Maybe (BlessedOp din m)
    editor _ _ _ _ _ = Nothing


instance Id.HasInput i Number is' is => HasEditor' (CliD Number) (Node f nstate is os m) i is' is Number m where
    editor' :: Proxy (CliD Number) -> Proxy is -> Proxy is' -> NodeBoxKey -> Id.Input i -> Node f nstate is os m -> Maybe (BlessedOp Number m)
    editor' _ _ _ key input node = Just $ ENumber.editor key node input
else instance Id.HasInput i din is' is => HasEditor' (CliD din) (Node f nstate is os m) i is' is din m where
    editor' :: Proxy (CliD din) -> Proxy is -> Proxy is' -> NodeBoxKey -> Id.Input i -> Node f nstate is os m  -> Maybe (BlessedOp din m)
    editor' _ _ _ _ _ _ = Nothing
-}


editors =
    []


createEditorFor :: forall m r. MonadEffect m => WrapRepr -> (WrapRepr -> Effect Unit) -> Maybe (RawNodeKey /\ BlessedOp (HasEditors r) m) -- (String /\ Blessed WrapRepr) -- (BlessedOp WrapRepr m)
createEditorFor (H.Value (H.Number n)) fn = Just $ ENumber.editor n $ fn
createEditorFor _ _ = Nothing


editorIdOf :: WrapRepr -> Maybe EditorId -- (BlessedOp WrapRepr m)
editorIdOf (H.Value (H.Number _)) = Just $ EditorId "number"
editorIdOf _ = Nothing