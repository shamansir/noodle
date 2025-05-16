module Web.Components.ValueEditor where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.Maybe (fromMaybe) as M

import Noodle.Id as Id
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Halogen (Component, RefLabel) as H

newtype EditorId = EditorId String

derive instance Eq EditorId
derive instance Ord EditorId


type Def repr =
    { inlet :: Id.InletR
    , pos :: { x :: Number, y :: Number }
    , editor :: EditorId
    , currentValue :: ValueInChannel repr
    }


data Input = Input


data Output v
    = SendValue v


{- type ValueEditor v state m
    =  v -- initial value
    -> (v -> Effect Unit) -- send value
    -> ValueEditorComp v state m
-}


data Query a
    = Query a


type ValueEditor v state m = ValueEditorComp v state m


type ValueEditorComp v state m =
    H.Component Query Input (Output v) m
{-
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
-}