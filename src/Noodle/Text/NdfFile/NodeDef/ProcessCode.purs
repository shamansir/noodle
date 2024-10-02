module Noodle.Text.NdfFile.NodeDef.ProcessCode where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Text.Format as T

import Type.Proxy (Proxy)
import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode, NDF, PS)
import Noodle.Ui.Cli.Tagging as F


data ProcessCode
    = NoneSpecified
    | Raw String
    | Auto String
    | JS String


derive instance Eq ProcessCode


contents :: ProcessCode -> String
contents = case _ of
    NoneSpecified -> ""
    Raw rawContents -> rawContents
    Auto autoContents -> autoContents
    JS jsContents -> jsContents



startMarker :: ProcessCode -> String
startMarker = case _ of
    NoneSpecified -> ""
    Raw _ -> "#-|"
    Auto _ -> "/-|"
    JS _ -> "$-|"


endMarker :: ProcessCode -> String
endMarker = case _ of
    NoneSpecified -> ""
    Raw _ -> "|-#"
    Auto _ -> "|-/"
    JS _ -> "|-$"


markersFor :: ProcessCode -> { start :: String, end :: String }
markersFor pc = { start : startMarker pc, end : endMarker pc }


process :: ProcessCode -> String
process = contents -- FIXME


instance ToCode NDF opts ProcessCode where
    toCode :: Proxy NDF -> opts -> ProcessCode -> String
    toCode _ _ pc = case pc of
        NoneSpecified -> ""
        _ -> startMarker pc <> contents pc <> endMarker pc


instance ToTaggedCode NDF opts ProcessCode where
    toTaggedCode :: Proxy NDF -> opts -> ProcessCode -> T.Tag
    toTaggedCode _ _ pc =
        case pc of
            NoneSpecified -> T.s ""
            _ -> F.operator (startMarker pc) <> T.s (contents pc) <> F.operator (endMarker pc)