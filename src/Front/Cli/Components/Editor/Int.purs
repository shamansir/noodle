module Cli.Components.Editor.Int where

import Prelude

import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Maybe (Maybe, fromMaybe)
import Data.Int (fromString) as Int

import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE

import Cli.Components.Editor.Textual (fromKey) as Textual

import Cli.Keys (intValueEditor, IntValueEditorKey) as Key


iveKey :: Key.IntValueEditorKey
iveKey = Key.intValueEditor


editor :: forall state m. MonadThrow Error m => ValueEditor (Maybe Int) state m
editor = VE.imap Int.fromString (map show >>> fromMaybe "-") $ Textual.fromKey iveKey