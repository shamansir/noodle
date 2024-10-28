module Noodle.Text.NdfFile where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String
import Data.Map (Map)
import Data.Map (empty, insert, update) as Map
import Data.Foldable (foldr)
import Data.Newtype (class Newtype)

import Data.Text.Format as T

import Noodle.Id (FamilyR) as Id
import Noodle.Text.NdfFile.Command (Command(..), commandsToNdf, commandsToTaggedNdf)
import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Text.NdfFile.NodeDef (NodeDef, ProcessAssign(..))
import Noodle.Text.NdfFile.NodeDef (forceAssign) as ND
import Noodle.Text.NdfFile.Types (NodeFamily)
import Noodle.Text.NdfFile.NodeDef (group, fnDef, family) as ND
import Noodle.Ui.Cli.Tagging as T


currentVersion = 0.2


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
toNdfCode (NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) failedLines commands) =
    toolkit <> " "
    <> show toolkitVersion
    <> (if ndfVersion == 0.1 then "\n" else " " <> show ndfVersion <> "\n")
    <> commandsToNdf commands


toTaggedNdfCode :: NdfFile -> T.Tag
toTaggedNdfCode (NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) failedLines commands) =
    T.toolkit toolkit <> T.space
    <> T.tkVersion toolkitVersion <> T.space
    <> T.ndfVersion ndfVersion <> T.nl
    <> (if ndfVersion == 0.1 then T.nl else T.space <> T.ndfVersion ndfVersion <> T.nl)
    <> commandsToTaggedNdf commands


extractCommands :: NdfFile -> Array Command
extractCommands (NdfFile _ _ commands) = commands


failedLines :: NdfFile -> Array FailedLine
failedLines (NdfFile _ failedLines' _) = failedLines'


loadDefinitions :: NdfFile -> Map NodeFamily NodeDef
loadDefinitions = extractCommands >>> foldr applyCommand Map.empty
    where
        applyCommand :: Command -> Map NodeFamily NodeDef -> Map NodeFamily NodeDef
        applyCommand cmd theMap =
            case cmd of
                DefineNode nodeDef ->
                    Map.insert (ND.family nodeDef) nodeDef theMap
                AssignProcess (ProcessAssign (family /\ processCode)) ->
                    Map.update (ND.forceAssign processCode >>> Just) family theMap
                _ -> theMap