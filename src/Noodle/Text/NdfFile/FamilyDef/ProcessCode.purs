module Noodle.Text.NdfFile.FamilyDef.ProcessCode where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Text.Format as T
import Data.String (contains, split, trim, indexOf, splitAt, drop, joinWith, Pattern(..)) as String
import Data.String.CodeUnits (singleton, dropRight) as String
import Data.String.Regex (Regex, match, regex, replace, search) as RGX
import Data.String.Regex.Flags (global, noFlags) as RGX
import Data.Foldable (foldl)
import Data.Array (snoc)
import Data.Array (catMaybes, length, nub) as Array
import Data.Array.NonEmpty (toArray) as NEA
import Data.String.Extra2 (linesCount) as String

import Parsing (Parser) as P
import Parsing.String (string)  as P
import Parsing.String.Extra (anythingBut, eol) as P
import Parsing.Combinators (between, choice) as P

import Noodle.Text.Code.Target (NDF, PS)
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode)
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


type AutoData_ = { allInlets :: Array String, sends :: Array { mbOut :: Maybe String, expr :: String } }


_processAutoCode :: String -> String
_processAutoCode src =
    let
        indent = "  "

        eOutNameRegex = RGX.regex "^([\\w\\d-]+)::" RGX.noFlags
        eInletsRegex = RGX.regex "<([\\w\\d-]+)>" RGX.global

        searchFunc :: RGX.Regex -> RGX.Regex -> String -> AutoData_ -> AutoData_
        searchFunc inletsRegex outNameRegex test collectedData =
            let
                trimmed = String.trim test
                toInletName str = String.dropRight 1 $ String.drop 1 str
                findInlets definition =
                    case RGX.match inletsRegex definition of
                        Just matches -> Array.catMaybes $ map toInletName <$> NEA.toArray matches
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
                                        collectedData.sends
                                        `snoc` { mbOut : Just before, expr : (replaceInlets $ String.drop 2 after) }
                                    }
                        Nothing ->
                                { allInlets : collectedData.allInlets <> findInlets trimmed
                                , sends :
                                    collectedData.sends
                                    `snoc` { mbOut : Nothing, expr : replaceInlets trimmed }
                                }
                else
                    { allInlets :
                        collectedData.allInlets
                        <> findInlets trimmed
                    , sends :
                        collectedData.sends
                        `snoc` { mbOut : Nothing, expr : replaceInlets trimmed }
                    }

        collectInfo :: String -> AutoData_ -> AutoData_
        collectInfo test collectedData =
            case (searchFunc <$> eInletsRegex <*> eOutNameRegex) of
                Right f -> f test collectedData
                Left _ -> collectedData

        inletStr inlet = indent <> inlet <> " <- " <> "Fn.receive _in_" <> inlet
        sendStr { mbOut, expr } =
            case mbOut of
                Just out -> indent <> "Fn.send _out_" <> out <> " $ " <> expr
                Nothing -> indent <> expr

        toExpression :: AutoData_ -> String
        toExpression { allInlets, sends } = "do\n" <>
            if (Array.length allInlets > 0) then
                (String.joinWith "\n" $ inletStr <$> Array.nub allInlets)
                <>
                (if Array.length sends > 0
                    then "\n" <> String.joinWith "\n" (sendStr <$> sends)
                    else ""
                )
            else
                if (Array.length sends > 0) then
                    String.joinWith "\n" $ sendStr <$> sends
                else
                    src

    in
        src
            # String.split (String.Pattern ";")
            # foldl (flip collectInfo) { allInlets : [], sends : [ ] }
            # toExpression


process :: ProcessCode -> String
process = case _ of
    NoneSpecified -> "{- EMPTY PROCESS -}\n    pure unit"
    Raw str -> str
    Auto str -> _processAutoCode str
    JS code -> "fromJsCode $ jsCode $\n\t\t\"\"\"" <> code <> "\n\t\t\"\"\""


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


instance ToCode PS opts ProcessCode where
    toCode :: Proxy PS -> opts -> ProcessCode -> String
    toCode _ _ = process


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


ndfLinesCount :: ProcessCode -> Int
ndfLinesCount = case _ of
    NoneSpecified -> 0
    Raw raw -> String.linesCount raw
    Auto auto -> String.linesCount auto
    JS js -> String.linesCount js