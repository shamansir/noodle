module Toolkit.Hydra2.Family.Render.Cli where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Maybe (Maybe(..))

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody, class HasBody', class HasCustomSize, class HasEditor)

import Cli.Keys (NodeBoxKey)

import Blessed.Internal.BlessedOp (BlessedOp)

import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, State, Node) as FNumber
import Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber (render) as FNumber
import Toolkit.Hydra2.Family.Out.FOut (Inputs, Outputs, State, Node) as FOut
import Toolkit.Hydra2.Family.Render.Cli.Out.FOut (render) as FOut
import Toolkit.Hydra2.Family.Display.FInfo (Inputs, Outputs, State, Node) as FInfo
import Toolkit.Hydra2.Family.Render.Cli.Display.FInfo (render) as FInfo
import Toolkit.Hydra2.Family.Feed.FFn (Inputs, Outputs, State, Node) as FFn
import Toolkit.Hydra2.Family.Render.Cli.Feed.FFn (render) as FFn
import Toolkit.Hydra2.Family.Feed.FArray (Inputs, Outputs, State, Node) as FArray
import Toolkit.Hydra2.Family.Render.Cli.Feed.FArray (render) as FArray

import Toolkit.Hydra2.Repr.Wrap (WrapRepr)

import Noodle.Node2 (Node)
import Noodle.Id as Id


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
else instance (Applicative m, MonadEffect m) => HasBody (CliF "fn") "fn" FFn.State FFn.Inputs FFn.Outputs m where
    run :: Proxy (CliF "fn") -> NodeBoxKey -> FFn.Node m -> BlessedOp FFn.State m
    run _ = FFn.render
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
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "fn") (FFn.Node m) FFn.State m where
    run' :: Proxy (CliF "fn") -> NodeBoxKey -> FFn.Node m -> BlessedOp FFn.State m
    run' _ = FFn.render
else instance (Applicative m, MonadEffect m) => HasBody' (CliF "array") (FArray.Node m) FArray.State m where
    run' :: Proxy (CliF "array") -> NodeBoxKey -> FArray.Node m -> BlessedOp FArray.State m
    run' _ = FArray.render
else instance HasBody' (CliF f) (Node f state is os m) state m where
    run' :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
    run' _ _ _ = pure unit


instance HasCustomSize (CliF "fn") (FFn.Node m) where
    size :: Proxy (CliF "fn") -> NodeBoxKey -> FFn.Node m -> Maybe { width :: Int, height :: Int }
    size _ _ _ = Just { width : 15, height : 10 }
else instance HasCustomSize (CliF f) (Node f state is os m) where
    size :: Proxy (CliF f) -> NodeBoxKey -> Node f state is os m -> Maybe { width :: Int, height :: Int }
    size _ _ _ = Nothing


instance HasEditor (CliD din) (Id.Input i) (Node f nstate is os Effect) din Effect where
    editor :: Proxy (CliD din) -> NodeBoxKey -> Id.Input i -> Node f nstate is os Effect -> Maybe (BlessedOp din Effect)
    editor _ _ _ _ = Nothing