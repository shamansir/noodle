module Cli.Components.Editor.Color where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Number (fromString) as Number

import Cli.Components.Editor.Textual
import Cli.Components.ValueEditor (ValueEditor)
import Cli.Components.ValueEditor (imap) as VE

import Cli.Components.Editor.Textual (editor) as Textual

import Color (Color)
import Color (toHexString, fromHexString) as Color


editor :: forall state m. ValueEditor (Maybe Color) state m
editor = VE.imap Color.fromHexString (map Color.toHexString >>> fromMaybe "-") Textual.editor