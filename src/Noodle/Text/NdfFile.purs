module Noodle.Text.NdfFile where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (sortWith, length) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map (empty, insert, update) as Map
import Data.Foldable (foldl)
import Data.Newtype (class Newtype)

import Data.Text.Format (Tag, nl, space) as T

import Noodle.Text.NdfFile.Command (Command(..), commandsToNdf, commandsToTaggedNdf)
import Noodle.Text.NdfFile.Command (priority) as Command
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Text.NdfFile.NodeDef (NodeDef, ProcessAssign(..))
import Noodle.Text.NdfFile.Types (NodeFamily)
import Noodle.Text.NdfFile.NodeDef (family, forceAssign) as ND
import Noodle.Ui.Cli.Tagging (ndfVersion, tkVersion, toolkit) as T


currentVersion = 0.2 :: Number


newtype Header =
    Header
        { toolkit :: String
        , toolkitVersion :: Number
        , ndfVersion :: Number
        }


data NdfFile =
    NdfFile
        Header
        (Array FailedLine)
        (Array Command)


derive instance Eq Header
derive instance Eq NdfFile


newtype FailedLine = FailedLine String


derive instance Newtype FailedLine _
derive newtype instance Eq FailedLine
derive newtype instance Show FailedLine


instance Show NdfFile where
    show :: NdfFile -> String
    show = toNdfCode


instance ToCode NDF opts NdfFile where
    toCode :: Proxy NDF -> opts -> NdfFile -> String
    toCode = const $ const toNdfCode


instance ToTaggedCode NDF opts NdfFile where
    toTaggedCode :: Proxy NDF -> opts -> NdfFile -> T.Tag
    toTaggedCode = const $ const toTaggedNdfCode


init :: String -> Number -> NdfFile
init tk version = init_ { toolkit : tk, toolkitVersion : version, ndfVersion : currentVersion }


init_ :: { toolkit :: String
         , toolkitVersion :: Number
         , ndfVersion :: Number
         } -> NdfFile
init_ header = from_ header []


from :: String -> Number -> Array Command -> NdfFile
from tk version = from_ { toolkit : tk, toolkitVersion : version, ndfVersion : currentVersion }


from_ :: { toolkit :: String
         , toolkitVersion :: Number
         , ndfVersion :: Number
         } -> Array Command -> NdfFile
from_ header = NdfFile (Header header) []


append :: Command -> NdfFile -> NdfFile
append cmd (NdfFile header failedLines cmds) = NdfFile header failedLines $ cmd : cmds


toNdfCode :: NdfFile -> String
toNdfCode (NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) _ commands) =
    toolkit <> " "
    <> show toolkitVersion
    <> (if ndfVersion == 0.1 then "\n" else " " <> show ndfVersion <> "\n")
    <> commandsToNdf commands


toTaggedNdfCode :: NdfFile -> T.Tag
toTaggedNdfCode (NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) _ commands) =
    T.toolkit toolkit <> T.space
    <> T.tkVersion toolkitVersion <> T.space
    <> T.ndfVersion ndfVersion <> T.nl
    <> (if ndfVersion == 0.1 then T.nl else T.space <> T.ndfVersion ndfVersion <> T.nl)
    <> commandsToTaggedNdf commands


extractCommands :: NdfFile -> Array Command
extractCommands (NdfFile _ _ commands) = commands


failedLines :: NdfFile -> Array FailedLine
failedLines (NdfFile _ failedLines' _) = failedLines'


hasFailedLines :: NdfFile -> Boolean
hasFailedLines = failedLines >>> Array.length >>> (_ > 0)


normalize :: NdfFile -> NdfFile
normalize (NdfFile header failedCommands commands) =
    NdfFile header failedCommands $ normalizeCommands commands


normalizeCommands :: Array Command -> Array Command
normalizeCommands = Array.sortWith Command.priority


loadDefinitions :: NdfFile -> Map NodeFamily NodeDef
loadDefinitions = extractCommands >>> normalizeCommands >>> foldl applyCommand Map.empty
    where
        applyCommand :: Map NodeFamily NodeDef -> Command -> Map NodeFamily NodeDef
        applyCommand theMap =
            case _ of
                DefineNode nodeDef ->
                    theMap # Map.insert (ND.family nodeDef) nodeDef
                AssignProcess (ProcessAssign (family /\ processCode)) ->
                    theMap # Map.update (ND.forceAssign processCode >>> Just) family
                _ -> theMap
