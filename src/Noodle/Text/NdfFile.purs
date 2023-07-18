module Noodle.Text.NdfFile where

import Prelude

import Type.Proxy (Proxy)

import Data.Array ((:))
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String

import Noodle.Text.NdfFile.Command (Command, commandsToNdf)

import Toolkit.Hydra2.Lang.ToCode (class ToCode, toCode, NDF, ndf)


newtype Header = Header (String /\ Number)


data NdfFile = NdfFile Header (Array Command)


instance Show NdfFile where
  show :: NdfFile -> String
  show (NdfFile (Header (tk /\ version)) commands) =
    tk <> " " <> show version <> "\n" <>
    (String.joinWith "\n" $ toCode ndf <$> commands)


instance ToCode NDF NdfFile where
    toCode :: Proxy NDF -> NdfFile -> String
    toCode = const toNdfCode


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


extractCommands :: NdfFile -> Array Command
extractCommands (NdfFile _ commands) = commands