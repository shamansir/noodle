module Cli.Components.Editor.Numeric where

import Prelude

import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Maybe (Maybe, fromMaybe)
import Data.Number (fromString) as Number

import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE

import Cli.Components.Editor.Textual (boundTo) as Textual

import Cli.Keys (numberValueEditor, NumberValueEditorKey) as Key


nveKey :: Key.NumberValueEditorKey
nveKey = Key.numberValueEditor


editor :: forall state m. MonadThrow Error m => ValueEditor (Maybe Number) state m
editor = VE.imap Number.fromString (map show >>> fromMaybe "-") $ Textual.boundTo nveKey