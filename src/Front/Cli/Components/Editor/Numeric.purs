module Cli.Components.Editor.Numeric where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Number (fromString) as Number

import Cli.Components.Editor.Textual
import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE

import Cli.Components.Editor.Textual (editor) as Textual


editor :: forall state m. ValueEditor (Maybe Number) state m
editor = VE.imap Number.fromString (map show >>> fromMaybe "-") Textual.editor