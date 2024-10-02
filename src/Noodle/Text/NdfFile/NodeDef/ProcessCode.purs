module Noodle.Text.NdfFile.NodeDef.ProcessCode where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Text.Format as T
import Data.String (contains, Pattern(..)) as String
import Data.String.CodeUnits (singleton) as String

import Parsing (Parser) as P
import Parsing.String (char, string)  as P
import Parsing.String.Basic (noneOf) as P
import Parsing.String.Extra as P
import Parsing.Token (alphaNum, space) as P
import Parsing.Combinators (between, choice, option, optionMaybe, sepBy, try) as P
import Parsing.Combinators ((<?>))

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
    Raw _  -> "#-|"
    Auto _ -> "/-|"
    JS _   -> "$-|"


endMarker :: ProcessCode -> String
endMarker = case _ of
    NoneSpecified -> ""
    Raw _  -> "|-#"
    Auto _ -> "|-/"
    JS _   -> "|-$"


startAltMarker :: ProcessCode -> String
startAltMarker = case _ of
    NoneSpecified -> ""
    Raw _  -> "%┤"
    Auto _ -> "{┤"
    JS _   -> "$┤"


endAltMarker :: ProcessCode -> String
endAltMarker = case _ of
    NoneSpecified -> ""
    Raw _  -> "├%"
    Auto _ -> "├}"
    JS _   -> "├$"


stopChar :: Char
stopChar = '|'


stopAltChar :: Char
stopAltChar = '├'


markersFor :: ProcessCode -> { start :: String, end :: String }
markersFor pc = { start : startMarker pc, end : endMarker pc }


process :: ProcessCode -> String
process = contents -- FIXME


instance ToCode NDF opts ProcessCode where
    toCode :: Proxy NDF -> opts -> ProcessCode -> String
    toCode _ _ pc = case pc of
        NoneSpecified -> ""
        _ ->
            if not $ String.contains (String.Pattern $ String.singleton stopChar) $ contents pc then
                startMarker pc <> contents pc <> endMarker pc
            else
                startAltMarker pc <> contents pc <> endAltMarker pc


instance ToTaggedCode NDF opts ProcessCode where
    toTaggedCode :: Proxy NDF -> opts -> ProcessCode -> T.Tag
    toTaggedCode _ _ pc =
        case pc of
            NoneSpecified -> T.s ""
            _ ->
                if not $ String.contains (String.Pattern $ String.singleton stopChar) $ contents pc then
                    F.operator (startMarker pc) <> T.s (contents pc) <> F.operator (endMarker pc)
                else
                    F.operator (startAltMarker pc) <> T.s (contents pc) <> F.operator (endAltMarker pc)


parser :: P.Parser String ProcessCode
parser =
  P.choice
    [ Raw <$> P.between
          (P.string "#-|")
          (P.string "|-#")
          (P.anythingBut stopChar)
    , Auto <$> P.between
          (P.string "/-|")
          (P.string "|-/")
          (P.anythingBut stopChar)
    , JS <$> P.between
          (P.string "$-|")
          (P.string "|-$")
          (P.anythingBut stopChar)
    , Raw <$> P.between
          (P.string "%┤")
          (P.string "├%")
          (P.anythingBut stopAltChar)
    , Auto <$> P.between
          (P.string "{┤")
          (P.string "├}")
          (P.anythingBut stopAltChar)
    , JS <$> P.between
          (P.string "$┤")
          (P.string "├}")
          (P.anythingBut stopAltChar)
    , P.eol *> pure NoneSpecified
    ]
