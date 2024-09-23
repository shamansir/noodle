module Noodle.Text.NdfFile where

import Prelude

import Type.Proxy (Proxy)

import Data.Array ((:))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String

import Data.Text.Format as T

import Noodle.Text.NdfFile.Command (Command, commandsToNdf, commandsToTaggedNdf)
import Noodle.Text.ToCode (class ToCode, toCode, class ToTaggedCode, toTaggedCode, NDF, ndf)
import Noodle.Ui.Cli.Tagging as T

newtype Header = Header (String /\ Number)


data NdfFile = NdfFile Header (Array Command)


derive instance Eq Header
derive instance Eq NdfFile


instance Show NdfFile where
  show :: NdfFile -> String
  show (NdfFile (Header (tk /\ version)) commands) =
    tk <> " " <> show version <> "\n" <>
    (String.joinWith "\n" $ toCode ndf <$> commands)


instance ToCode NDF NdfFile where
    toCode :: Proxy NDF -> NdfFile -> String
    toCode = const toNdfCode


instance ToTaggedCode NDF NdfFile where
    toTaggedCode :: Proxy NDF -> NdfFile -> T.Tag
    toTaggedCode = const toTaggedNdfCode


init :: String -> Number -> NdfFile
init tk version = NdfFile (Header $ tk /\ version) []


from :: String -> Number -> Array Command -> NdfFile
from tk version = NdfFile $ Header (tk /\ version)


append :: Command -> NdfFile -> NdfFile
append cmd (NdfFile header cmds) = NdfFile header $ cmd : cmds


toNdfCode :: NdfFile -> String
toNdfCode (NdfFile (Header (tk /\ version)) commands) =
    tk <> " " <> show version <> "\n" <>
    commandsToNdf commands


toTaggedNdfCode :: NdfFile -> T.Tag
toTaggedNdfCode (NdfFile (Header (tk /\ version)) commands) =
    T.toolkit tk <> T.s " " <> T.version version <> T.nl <>
    commandsToTaggedNdf commands


extractCommands :: NdfFile -> Array Command
extractCommands (NdfFile _ commands) = commands