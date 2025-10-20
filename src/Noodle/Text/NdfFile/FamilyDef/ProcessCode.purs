module Noodle.Text.NdfFile.FamilyDef.ProcessCode where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Functor.Extra ((<$$>))
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
import Noodle.Ui.Tagging as F


data Target = Raw | Typed
derive instance Eq Target


data ProcessCode
    = NoneSpecified
    | Copy String -- Copy the contents as it is
    | Encoded Target String -- Split with ';'. Replace every `<inlet-name>` with `Fn.receive`, send the result(-s) to given outlet(-s)
    | JS String -- Call given JS function (not yet implemented)


derive instance Eq ProcessCode


contents :: ProcessCode -> String
contents = case _ of
    NoneSpecified -> ""
    Copy rawContents -> rawContents
    Encoded _ encodedContents -> encodedContents
    JS jsContents -> jsContents


startMarker :: ProcessCode -> String
startMarker = case _ of
    NoneSpecified -> ""
    Copy _  -> "#-|"
    Encoded _ _ -> "/-|"
    JS _   -> "$-|"


endMarker :: ProcessCode -> String
endMarker = case _ of
    NoneSpecified -> ""
    Copy _  -> "|-#"
    Encoded _ _ -> "|-/"
    JS _   -> "|-$"


startAltMarker :: ProcessCode -> String
startAltMarker = case _ of
    NoneSpecified -> ""
    Copy _  -> "%┤"
    Encoded _ _ -> "{┤"
    JS _   -> "$┤"


endAltMarker :: ProcessCode -> String
endAltMarker = case _ of
    NoneSpecified -> ""
    Copy _  -> "├%"
    Encoded _ _ -> "├}"
    JS _   -> "├$"


stopChar :: Char
stopChar = '|'


stopAltChar :: Char
stopAltChar = '├'


markersFor :: ProcessCode -> { start :: String, end :: String }
markersFor pc = { start : startMarker pc, end : endMarker pc }


type EncodedData_ =
    { allInlets :: Array String
    , sends :: Array
        { mbOut :: Maybe String
        , expr :: String
        , localInlets :: Array String
        }
    }


newtype Indent = Indent String


_processEncodedCode :: Target -> Indent -> String -> String
_processEncodedCode target (Indent indent) src =
    let
        eOutNameRegex = RGX.regex "^([\\w\\d-]+)::" RGX.noFlags
        eInletsRegex = RGX.regex "<([\\w\\d-]+)>" RGX.global

        searchFunc :: RGX.Regex -> RGX.Regex -> String -> EncodedData_ -> EncodedData_
        searchFunc inletsRegex outNameRegex test collectedData =
            let
                trimmed = String.trim test
                toInletName str = String.dropRight 1 $ String.drop 1 str
                findInlets definition =
                    case RGX.match inletsRegex definition of
                        Just matches -> Array.catMaybes $ toInletName <$$> NEA.toArray matches
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
                                    let localInlets = findInlets $ String.drop 2 after in
                                    { allInlets : collectedData.allInlets <> localInlets
                                    , sends :
                                        collectedData.sends
                                        `snoc`
                                        { mbOut : Just before
                                        , expr : (replaceInlets $ String.drop 2 after)
                                        , localInlets
                                        }
                                    }
                        Nothing ->
                            let localInlets = findInlets trimmed in
                            { allInlets : collectedData.allInlets <> localInlets
                            , sends :
                                collectedData.sends
                                `snoc` { mbOut : Nothing, expr : replaceInlets trimmed, localInlets }
                            }
                else
                    let localInlets = findInlets trimmed in
                    { allInlets :
                        collectedData.allInlets
                        <> localInlets
                    , sends :
                        collectedData.sends
                        `snoc`
                        { mbOut : Nothing
                        , expr : replaceInlets trimmed
                        , localInlets
                        }
                    }

        collectInfo :: String -> EncodedData_ -> EncodedData_
        collectInfo test collectedData =
            case (searchFunc <$> eInletsRegex <*> eOutNameRegex) of
                Right f -> f test collectedData
                Left _ -> collectedData

        inletTypedStr inlet = indent <> inlet <> " <- " <> "Fn.receive _in_" <> inlet
        inletRawStr inlet = indent <> "vic_" <> inlet <> " <- " <> "RP.receive \"" <> inlet <> "\""
        inletValRawStr inlet = indent <> "  " <> inlet <> " <- " <> "vic_" <> inlet
        sendTypedStr { mbOut, expr } =
            case mbOut of
                Just out -> indent <> "Fn.send _out_" <> out <> " $ " <> expr
                Nothing -> indent <> expr
        sendRawStr { mbOut, expr, localInlets } =
            case mbOut of
                Just out -> indent <>
                    "RP.send \"" <> out <> "\" " <>
                    if Array.length localInlets <= 1 then
                        "vic_" <> expr
                    else
                        "$ do\n" <> (String.joinWith "\n" $ inletValRawStr <$> localInlets) <> "\n"
                        <> indent <> "  " <> "pure $ " <>  expr

                Nothing -> indent <> expr

        toEncoded :: EncodedData_ -> String
        toEncoded { allInlets, sends } = "do\n" <>
            case target of
                Typed ->

                    if (Array.length allInlets > 0) then
                        (String.joinWith "\n" $ inletTypedStr <$> Array.nub allInlets)
                        <>
                        (if Array.length sends > 0
                            then "\n" <> (String.joinWith "\n" $ sendTypedStr <$> sends)
                            else ""
                        )
                    else
                        if (Array.length sends > 0) then
                            String.joinWith "\n" $ sendTypedStr <$> sends
                        else
                            src -- FIXME: ??

                Raw ->

                    if (Array.length allInlets > 0) then
                        (String.joinWith "\n" $ inletRawStr <$> Array.nub allInlets)
                        <>
                        (if Array.length sends > 0
                            then "\n" <> (String.joinWith "\n" $ sendRawStr <$> sends)
                            else ""
                        )
                    else
                        if (Array.length sends > 0) then
                            String.joinWith "\n" $ sendRawStr <$> sends
                        else
                            src -- FIXME: ??


    in
        src
            # String.split (String.Pattern ";")
            # foldl (flip collectInfo) { allInlets : [], sends : [ ] }
            # toEncoded


process :: Indent -> ProcessCode -> String
process (Indent indent) = case _ of
    NoneSpecified -> "{- EMPTY PROCESS -}\n" <> indent <> "pure unit"
    Copy str -> str
    Encoded target str -> _processEncodedCode target (Indent indent) str
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
    toCode _ _ = process $ Indent "  " -- FIXME


parser :: P.Parser String ProcessCode
parser =
  P.choice
    [ Copy <$> P.between
          (P.string "#-|")
          (P.string "|-#")
          (P.anythingBut stopChar)
    , Encoded Typed <$> P.between
          (P.string "/-|")
          (P.string "|-/")
          (P.anythingBut stopChar)
    , JS <$> P.between
          (P.string "$-|")
          (P.string "|-$")
          (P.anythingBut stopChar)
    , Copy <$> P.between
          (P.string "%┤")
          (P.string "├%")
          (P.anythingBut stopAltChar)
    , Encoded Typed <$> P.between
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
    Copy raw -> String.linesCount raw
    Encoded _ auto -> String.linesCount auto
    JS js -> String.linesCount js