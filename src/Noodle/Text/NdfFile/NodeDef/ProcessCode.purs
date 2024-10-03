module Noodle.Text.NdfFile.NodeDef.ProcessCode where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Text.Format as T
import Data.String (contains, split, trim, indexOf, splitAt, drop, joinWith, Pattern(..)) as String
import Data.String.CodeUnits (singleton) as String
import Data.String.Regex as RGX
import Data.String.Regex.Flags as RGX
import Data.Foldable (foldr)
import Data.Array ((:))
import Data.Array (catMaybes) as Array
import Data.Array.NonEmpty (toArray) as NEA

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
    | Raw String -- Copy the contents as it is
    | Auto String -- Split with ';'. Replace every `<inlet-name>` with `Fn.receive`, send the result(-s) to given outlet(-s)
    | JS String -- Call given JS function (not yet implemented)


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


type AutoData_ = { allInlets :: Array String, sends :: Array { out :: Maybe String, expr :: String } }


_processAutoCode :: String -> String
_processAutoCode =
    let
        eOutNameRegex = RGX.regex "^([\\w\\d-]+)::" RGX.noFlags
        eInletsRegex = RGX.regex "<([\\w\\d-]+)>" RGX.global

        searchFunc :: RGX.Regex -> RGX.Regex -> String -> AutoData_ -> AutoData_
        searchFunc inletsRegex outNameRegex test collectedData =
            let
                trimmed = String.trim test
                findInlets definition =
                    case RGX.match inletsRegex definition of
                        Just matches -> Array.catMaybes $ NEA.toArray matches
                        Nothing -> []
                replaceInlets definition =
                    RGX.replace inletsRegex "$1" definition
                outletNamePos = fromMaybe (-1) $ RGX.search outNameRegex trimmed
            in
                if outletNamePos == 0 then
                    case String.indexOf (String.Pattern "::") trimmed of
                        Just doubleColonIdx ->
                            case String.splitAt doubleColonIdx trimmed of
                                { before, after } ->
                                    { allInlets : collectedData.allInlets <> (findInlets $ String.drop 2 after)
                                    , sends :
                                        { out : Just before, expr : (replaceInlets $ String.drop 2 after) }
                                        : collectedData.sends
                                    }
                        Nothing ->
                                { allInlets : collectedData.allInlets <> findInlets trimmed
                                , sends :
                                    { out : Nothing, expr : replaceInlets trimmed }
                                    : collectedData.sends
                                }
                else
                    { allInlets : collectedData.allInlets <> findInlets trimmed
                    , sends :
                        { out : Nothing, expr : replaceInlets trimmed }
                        : collectedData.sends
                    }

        collectInfo :: String -> AutoData_ -> AutoData_
        collectInfo test collectedData =
            case (searchFunc <$> eInletsRegex <*> eOutNameRegex) of
                Right f -> f test collectedData
                Left _ -> collectedData

        toExpression :: AutoData_ -> String
        toExpression { allInlets, sends } =
            String.joinWith "\n" allInlets <> String.joinWith "\n" (_.expr <$> sends)
    in
        String.split (String.Pattern ";")
            >>> foldr collectInfo { allInlets : [], sends : [ ] }
            >>> toExpression


process :: ProcessCode -> String
process = case _ of
    NoneSpecified -> ""
    Raw str -> str
    Auto str -> _processAutoCode str
    JS code -> code -- TODO


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
