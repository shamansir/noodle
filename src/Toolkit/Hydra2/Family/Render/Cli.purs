module Toolkit.Hydra2.Family.Render.Cli where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

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
import Toolkit.Hydra2.Family.Feed.FExpression (Inputs, Outputs, State, Node) as FExpression
import Toolkit.Hydra2.Family.Render.Cli.Feed.FExpression (render) as FExpression
import Toolkit.Hydra2.Family.Feed.FArray (Inputs, Outputs, State, Node) as FArray
import Toolkit.Hydra2.Family.Render.Cli.Feed.FArray (render) as FArray
import Toolkit.Hydra2.Family.Feed.FCallGlslFunction (Inputs, Outputs, State, Node) as FCallGlslFunction
import Toolkit.Hydra2.Family.Render.Cli.Node.Feed.CallGlslFunction (render) as FCallGlslFunction
import Toolkit.Hydra2.Family.CAI.FProductPalette (Inputs, Outputs, State, Node) as FProductPalette
import Toolkit.Hydra2.Family.Render.Cli.Node.CAI.FProductPalette (render, size) as FProductPalette

import Toolkit.Hydra2.Family.Render.Cli.Editor.Number as ENumber

import Toolkit.Hydra2.Repr.Wrap (WrapRepr)
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..)) as H
import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Glsl as Glsl

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
else instance (Applicative m, MonadEffect m) => HasBody (CliF "expression") "expression" FExpression.State FExpression.Inputs FExpression.Outputs m where
    run :: Proxy (CliF "expression") -> NodeBoxKey -> FExpression.Node m -> BlessedOp FExpression.State m
    run _ = FExpression.render
else instance (Applicative m, MonadEffect m) => HasBody (CliF "array") "array" FArray.State FArray.Inputs FArray.Outputs m where
    run :: Proxy (CliF "array") -> NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
    run _ = FArray.render
else instance (Applicative m, MonadEffect m) => HasBody (CliF "callFunction") "callFunction" FCallGlslFunction.State FCallGlslFunction.Inputs FCallGlslFunction.Outputs m where
    run :: Proxy (CliF "callFunction") -> NodeBoxKey -> FCallGlslFunction.Node m -> BlessedOp FCallGlslFunction.State m
    run _ = FCallGlslFunction.render
else instance (Applicative m, MonadEffect m) => HasBody (CliF "caiProductPalette") "caiProductPalette" FProductPalette.State FProductPalette.Inputs FProductPalette.Outputs m where
    run :: Proxy (CliF "caiProductPalette") -> NodeBoxKey -> FProductPalette.Node m -> BlessedOp FProductPalette.State m
    run _ = FProductPalette.render
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
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "expression") (FExpression.Node m) FExpression.State m where
    run' :: Proxy (CliF "expression") -> NodeBoxKey -> FExpression.Node m -> BlessedOp FExpression.State m
    run' _ = FExpression.render
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "array") (FArray.Node m) FArray.State m where
    run' :: Proxy (CliF "array") -> NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
    run' _ = FArray.render
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "callFunction") (FCallGlslFunction.Node m) FCallGlslFunction.State m where
    run' :: Proxy (CliF "callFunction") -> NodeBoxKey -> FCallGlslFunction.Node m -> BlessedOp FCallGlslFunction.State m
    run' _ = FCallGlslFunction.render
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "caiProductPalette") (FProductPalette.Node m) FProductPalette.State m where
    run' :: Proxy (CliF "caiProductPalette") -> NodeBoxKey -> FProductPalette.Node m -> BlessedOp FProductPalette.State m
    run' _ = FProductPalette.render
else instance HasBody' (CliF f) (Node f state is os m) state m where
    run' :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
    run' _ _ _ = pure unit


instance HasCustomSize (CliF "expression") (FExpression.Node m) where
    size :: Proxy (CliF "expression") -> NodeBoxKey -> FExpression.Node m -> Maybe { width :: Int, height :: Int }
    size _ _ _ = Just { width : 35, height : 5 }
else instance HasCustomSize (CliF "callFunction") (FCallGlslFunction.Node m) where
    size :: Proxy (CliF "callFunction") -> NodeBoxKey -> FCallGlslFunction.Node m -> Maybe { width :: Int, height :: Int }
    size _ _ _ = Just { width : 30, height : Array.length Glsl.knownFns + 2 }
else instance HasCustomSize (CliF "caiProductPalette") (FProductPalette.Node m) where
    size :: Proxy (CliF "caiProductPalette") -> NodeBoxKey -> FProductPalette.Node m -> Maybe { width :: Int, height :: Int }
    size _ _ node = FProductPalette.size node
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
createEditorFor (H.TOrV (H.V (H.Number n))) fn = Just $ ENumber.editor n $ fn
createEditorFor (H.TOrV _) fn = Just $ ENumber.editor 0.0 $ fn
createEditorFor _ _ = Nothing


editorIdOf :: WrapRepr -> Maybe EditorId -- (BlessedOp WrapRepr m)
editorIdOf (H.Value (H.Number _)) = Just $ EditorId "number"
editorIdOf (H.TOrV (H.V (H.Number _))) = Just $ EditorId "number"
editorIdOf (H.TOrV _) = Just $ EditorId "number"
editorIdOf _ = Nothing