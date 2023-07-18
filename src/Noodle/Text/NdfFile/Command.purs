module Noodle.Text.NdfFile.Command where

import Prelude (show, ($), (<$>), identity, class Eq)

import Data.Semigroup ((<>))
import Data.String as String
import Data.Array as Array

import Type.Proxy (Proxy)

import Toolkit.Hydra2.Lang.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode, NDF, ndf)

import Blessed.Tagger (Tag)
import Blessed.Tagger as T
import Cli.Tagging as T


data Command
    = MakeNode String Int Int String
    | Connect String Int String Int
    | Send String Int String
    | SendO String Int String
    | Connect_ String String String String
    | Send_ String String String
    | SendO_ String String String


derive instance Eq Command


instance ToCode NDF Command where
    toCode :: Proxy NDF -> Command -> String
    toCode _ =
        case _ of
            MakeNode family top left nodeId -> family <> " " <> show top <> " " <> show left <> " " <> nodeId
            Connect fromNode oindex toNode iindex -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> show iindex
            Send nodeId iindex value -> "-> " <> nodeId <> " " <> show iindex <> " " <> value
            SendO nodeId oindex value -> "~> " <> nodeId <> " " <> show oindex <> " " <> value
            Connect_ fromNode oname toNode iname -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> iname
            Send_ nodeId iname value -> "-> " <> nodeId <> " " <> iname <> " " <> value
            SendO_ nodeId oname value -> "~> " <> nodeId <> " " <> oname <> " " <> value


instance ToTaggedCode NDF Command where
    toTaggedCode :: Proxy NDF -> Command -> T.Tag -- TODO: move to Cli.Tagging module
    toTaggedCode _ =
        case _ of
            MakeNode family top left nodeId -> T.family family <> T.s " " <> T.coord top <> T.s " " <> T.coord left <> T.s " " <> T.nodeId nodeId
            Connect fromNode oindex toNode iindex -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outputIdx oindex <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inputIdx iindex
            Send nodeId iindex value -> T.operator "->" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.inputIdx iindex <> T.s " " <> T.value value
            SendO nodeId oindex value -> T.operator "~>" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.outputIdx oindex <> T.s " " <> T.value value
            Connect_ fromNode oname toNode iname -> T.operator "<>" <> T.s " " <> T.nodeId fromNode <> T.s " " <> T.outputId oname <> T.s " " <> T.nodeId toNode <> T.s " " <> T.inputId iname
            Send_ nodeId iname value -> T.operator "->" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.inputId iname <> T.s " " <> T.value value
            SendO_ nodeId oname value -> T.operator "~>" <> T.s " " <> T.nodeId nodeId <> T.s " " <> T.outputId oname <> T.s " " <> T.value value


-- instance ToCode NDF (Array Command) where
commandsToNdf :: Array Command -> String
commandsToNdf cmds = String.joinWith "\n" $ toCode ndf <$> (optimize $ Array.reverse cmds)


-- instance ToCode NDF (Array Command) where
commandsToTaggedNdf :: Array Command -> T.Tag
commandsToTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf <$> (optimize $ Array.reverse cmds)


optimize :: Array Command -> Array Command
optimize = identity -- TODO