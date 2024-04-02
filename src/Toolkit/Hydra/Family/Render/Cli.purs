module Tookit.Hydra.Family.Render.Cli where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasCliBody, class HasCliCustomSize) {-, class HasEditor, class HasEditor', class HasEditor'')-}

import Cli.Keys (NodeBoxKey)

import Blessed.Internal.Core (Blessed)
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (RawNodeKey, NodeKey, type (<^>))

import Tookit.Hydra.Family.Feed.FNumber (Inputs, Outputs, State, Node) as FNumber
import Tookit.Hydra.Family.Render.Cli.Feed.FNumber (render) as FNumber
import Tookit.Hydra.Family.Out.FOut (Inputs, Outputs, State, Node) as FOut
import Tookit.Hydra.Family.Render.Cli.Out.FOut (render) as FOut
import Tookit.Hydra.Family.Display.FInfo (Inputs, Outputs, State, Node) as FInfo
import Tookit.Hydra.Family.Render.Cli.Display.FInfo (render) as FInfo
import Tookit.Hydra.Family.Feed.FExpression (Inputs, Outputs, State, Node) as FExpression
import Tookit.Hydra.Family.Render.Cli.Feed.FExpression (render) as FExpression
import Tookit.Hydra.Family.Feed.FArray (Inputs, Outputs, State, Node) as FArray
import Tookit.Hydra.Family.Render.Cli.Feed.FArray (render) as FArray
import Tookit.Hydra.Family.Feed.FCallGlslFunction (Inputs, Outputs, State, Node) as FCallGlslFunction
import Tookit.Hydra.Family.Render.Cli.Node.Feed.CallGlslFunction (render) as FCallGlslFunction
import Tookit.Hydra.Family.CAI.FProductPalette (Inputs, Outputs, State, Node) as FProductPalette
import Tookit.Hydra.Family.Render.Cli.Node.CAI.FProductPalette (render, size) as FProductPalette

import Tookit.Hydra.Family.Render.Cli.Editor.Number as ENumber

import Tookit.Hydra.Repr.Wrap (WrapRepr)
import Tookit.Hydra.Repr.Wrap (WrapRepr(..)) as H
import Tookit.Hydra.Types as H
import Tookit.Hydra.Lang.Glsl as Glsl

import Noodle.Node (Node)
import Noodle.Id as Id


import Tookit.Hydra.Family.Render.Editor (EditorId(..), HasEditors)
import Toolkit.Hydra.Family.Render.RenderTarget


-- TODO: kind: data RenderTarget


--data CliF :: Symbol -> RenderTarget -> Type
data CliF (f :: Symbol) = CliF



-- data RendersToCli :: Cli

-- newtype RendersToCli = R (RendersTo Cli)


-- data CliD din = CliD


--foreign import data HH :: Symbol -> RenderTarget -> RenderItem


{- instance (Applicative m, MonadEffect m) => HasBody (RendersFamily Cli "number") "number" FNumber.State FNumber.Inputs FNumber.Outputs m where
    run :: Proxy _ -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    run _ = FNumber.render -}


{-
instance (Applicative m, MonadEffect m) => HasCliBody' (HH "number" Cli) "number" FNumber.State FNumber.Inputs FNumber.Outputs m where
    run'' :: Proxy _ -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    run'' _ = FNumber.render
-}


{-
instance (Applicative m, MonadEffect m) => HasBody (RendersTo Cli) "number" FNumber.State FNumber.Inputs FNumber.Outputs m where
    run :: Proxy _ -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    run _ = FNumber.render
else instance (MonadRec m, MonadEffect m) => HasBody (RendersTo Cli) "out" FOut.State FOut.Inputs FOut.Outputs m where
    run :: Proxy _ -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
    run _ = FOut.render
-}


{-
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
-}


instance (Applicative m, MonadEffect m) => HasCliBody (CliF "number") (FNumber.Node m) FNumber.State m where
    runBlessed :: Proxy (CliF "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    runBlessed _ = FNumber.render
else instance (MonadRec m, MonadEffect m) => HasCliBody (CliF "out") (FOut.Node m) FOut.State m where
    runBlessed :: Proxy (CliF "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
    runBlessed _ = FOut.render
else instance (MonadRec m, MonadEffect m) => HasCliBody (CliF "info") (FInfo.Node m) FInfo.State m  where
    runBlessed :: Proxy (CliF "info") -> NodeBoxKey -> FInfo.Node m -> BlessedOp FInfo.State m
    runBlessed _ = FInfo.render
else instance (Applicative m, MonadEffect m) => HasCliBody (CliF "expression") (FExpression.Node m) FExpression.State m where
    runBlessed :: Proxy (CliF "expression") -> NodeBoxKey -> FExpression.Node m -> BlessedOp FExpression.State m
    runBlessed _ = FExpression.render
else instance (Applicative m, MonadEffect m) => HasCliBody (CliF "array") (FArray.Node m) FArray.State m where
    runBlessed :: Proxy (CliF "array") -> NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
    runBlessed _ = FArray.render
else instance (Applicative m, MonadEffect m) => HasCliBody (CliF "callFunction") (FCallGlslFunction.Node m) FCallGlslFunction.State m where
    runBlessed :: Proxy (CliF "callFunction") -> NodeBoxKey -> FCallGlslFunction.Node m -> BlessedOp FCallGlslFunction.State m
    runBlessed _ = FCallGlslFunction.render
else instance (Applicative m, MonadEffect m) => HasCliBody (CliF "caiProductPalette") (FProductPalette.Node m) FProductPalette.State m where
    runBlessed :: Proxy (CliF "caiProductPalette") -> NodeBoxKey -> FProductPalette.Node m -> BlessedOp FProductPalette.State m
    runBlessed _ = FProductPalette.render
else instance HasCliBody (CliF f) (Node f state is os m) state m where
    runBlessed :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
    runBlessed _ _ _ = pure unit


instance HasCliCustomSize (CliF "expression") (FExpression.Node m) where
    cliSize :: Proxy (CliF "expression") -> NodeBoxKey -> FExpression.Node m -> Maybe { width :: Int, height :: Int }
    cliSize _ _ _ = Just { width : 35, height : 5 }
else instance HasCliCustomSize (CliF "callFunction") (FCallGlslFunction.Node m) where
    cliSize :: Proxy (CliF "callFunction") -> NodeBoxKey -> FCallGlslFunction.Node m -> Maybe { width :: Int, height :: Int }
    cliSize _ _ _ = Just { width : 30, height : Array.length Glsl.knownFns + 2 }
else instance HasCliCustomSize (CliF "caiProductPalette") (FProductPalette.Node m) where
    cliSize :: Proxy (CliF "caiProductPalette") -> NodeBoxKey -> FProductPalette.Node m -> Maybe { width :: Int, height :: Int }
    cliSize _ _ node = FProductPalette.size node
else instance HasCliCustomSize (CliF f) (Node f state is os m) where
    cliSize :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> Maybe { width :: Int, height :: Int }
    cliSize _ _ _ = Nothing



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