module Cli.Components.ValueEditor where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.Maybe (fromMaybe) as M

import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Repr.HasFallback (class HasFallback, fallback)


newtype EditorId = EditorId String


type ValueEditor v state m
    =  v -- initial value
    -> (v -> Effect Unit) -- send value
    -> ValueEditorComp v state m



type ValueEditorComp v state m =
    { create :: BlessedOp state m
    , inject :: v -> BlessedOp state m
    , transpose :: { x :: Int, y :: Int } -> BlessedOp state m
    }


imap :: forall a b state m. (a -> b) -> (b -> a) -> ValueEditor a state m -> ValueEditor b state m
imap aToB bToA editorF =
    \curValue sendValue ->
        editorF (bToA curValue) (aToB >>> sendValue)
            # \editor -> editor { inject = bToA >>> editor.inject }


toMaybe :: forall v state m. HasFallback v => ValueEditor v state m -> ValueEditor (Maybe v) state m
toMaybe = imap pure $ M.fromMaybe fallback


fromMaybe :: forall v state m. HasFallback v => ValueEditor (Maybe v) state m -> ValueEditor v state m
fromMaybe = imap (M.fromMaybe fallback) pure