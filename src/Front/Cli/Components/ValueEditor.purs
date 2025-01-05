module Cli.Components.ValueEditor where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe)
import Data.Maybe (fromMaybe) as M
import Data.Tuple.Nested ((/\), type (/\))

import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)

import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.ChRepr (class FromToChRepr, toChRepr, fromChRepr)
import Noodle.Repr.ChRepr (wrap, unwrap) as Repr


newtype EditorId = EditorId String


type ValueEditor v state m = (MonadEffect m => v -> (v -> Effect Unit) -> NK.RawNodeKey /\ BlessedOp state m)


imap :: forall a b state m. (a -> b) -> (b -> a) -> ValueEditor a state m -> ValueEditor b state m
imap aToB bToA editorF =
    \curValue sendValue ->
        editorF (bToA curValue) (aToB >>> sendValue)


toMaybe :: forall v state m. HasFallback v => ValueEditor v state m -> ValueEditor (Maybe v) state m
toMaybe = imap pure $ M.fromMaybe fallback


fromMaybe :: forall v state m. HasFallback v => ValueEditor (Maybe v) state m -> ValueEditor v state m
fromMaybe = imap (M.fromMaybe fallback) pure


toReprable :: forall v state repr m. HasFallback repr => HasFallback v => FromToChRepr v repr => ValueEditor v state m -> ValueEditor repr state m
toReprable = fromMaybe <<< toMaybeReprable


toMaybeReprable :: forall v state repr m. HasFallback v => FromToChRepr v repr => ValueEditor v state m -> ValueEditor (Maybe repr) state m
toMaybeReprable = imap (flip bind toChRepr >>> map Repr.unwrap) (map Repr.wrap >>> flip bind fromChRepr) <<< toMaybe