module Cli.Components.ValueEditor where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.Maybe (fromMaybe) as M
import Data.Tuple.Nested ((/\), type (/\))

import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Repr.HasFallback (class HasFallback, fallback)


newtype EditorId = EditorId String


type ValueEditor v state m = v -> (v -> Effect Unit) -> NK.RawNodeKey /\ BlessedOp state m


imap :: forall a b state m. (a -> b) -> (b -> a) -> ValueEditor a state m -> ValueEditor b state m
imap aToB bToA editorF =
    \curValue sendValue ->
        editorF (bToA curValue) (aToB >>> sendValue)


toMaybe :: forall v state m. HasFallback v => ValueEditor v state m -> ValueEditor (Maybe v) state m
toMaybe = imap pure $ M.fromMaybe fallback


fromMaybe :: forall v state m. HasFallback v => ValueEditor (Maybe v) state m -> ValueEditor v state m
fromMaybe = imap (M.fromMaybe fallback) pure


{-
toReprable :: forall v state repr m. HasFallback repr => HasFallback v => FromToValueInChannel v repr => ValueEditor v state m -> ValueEditor repr state m
toReprable = fromMaybe <<< toChanneledRepr


toChanneledRepr :: forall v state repr m. FromToValueInChannel v repr => ValueEditor (Maybe v) state m -> ValueEditor (ValueInChannel repr) state m
toChanneledRepr = imap fromValue toValue
    where
        fromValue :: Maybe v -> ValueInChannel repr
        fromValue mbV = case (fromValueInChannel <$> mbV) of
            Just repr -> ViC.accept repr
            Nothing -> ViC.decline
        toValue :: ValueInChannel repr -> Maybe v
        toValue = ViC.toMaybe >>> map toValueInChannel >>> flip bind ViC.toMaybe
-}