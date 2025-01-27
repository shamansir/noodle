module Cli.Components.Editor.Color where

import Prelude

import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Maybe (Maybe, fromMaybe)

import Cli.Keys (colorValueEditor, ColorValueEditorKey) as Key

import Type.Data.Symbol (class IsSymbol)

import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE

import Cli.Components.Editor.Textual (boundTo) as Textual

import Color (Color)
import Color (toHexString, fromHexString) as Color


cveKey :: Key.ColorValueEditorKey
cveKey = Key.colorValueEditor


editor :: forall state m. MonadThrow Error m => ValueEditor (Maybe Color) state m
editor = VE.imap Color.fromHexString (map Color.toHexString >>> fromMaybe "-") $ Textual.boundTo cveKey