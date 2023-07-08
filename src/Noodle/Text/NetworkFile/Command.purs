module Noodle.Text.NetworkFile.Command where

import Prelude (show, ($), (<$>))

import Data.Semigroup ((<>))
import Data.String as String
import Data.Array as Array

import Type.Proxy (Proxy)

import Toolkit.Hydra2.Lang.ToCode (class ToCode, toCode, NDF, ndf)

data Command
    = Header String String
    | MakeNode String Int Int String
    | Connect String Int String Int
    | Send String Int String
    | SendO String Int String
    | Connect_ String String String String
    | Send_ String String String
    | SendO_ String String String


instance ToCode NDF Command where
    toCode :: Proxy NDF -> Command -> String
    toCode _ =
        case _ of
            Header toolkit version -> toolkit <> " " <> version
            MakeNode family top left nodeId -> family <> " " <> show top <> " " <> show left <> " " <> nodeId
            Connect fromNode oindex toNode iindex -> "<> " <> fromNode <> " " <> show oindex <> " " <> toNode <> " " <> show iindex
            Send nodeId iindex value -> "-> " <> nodeId <> " " <> show iindex <> " " <> value
            SendO nodeId oindex value -> "~> " <> nodeId <> " " <> show oindex <> " " <> value
            Connect_ fromNode oname toNode iname -> "<> " <> fromNode <> " " <> oname <> " " <> toNode <> " " <> iname
            Send_ nodeId iname value -> "-> " <> nodeId <> " " <> iname <> " " <> value
            SendO_ nodeId oname value -> "~> " <> nodeId <> " " <> oname <> " " <> value


-- instance ToCode NDF (Array Command) where
commandsToNdf :: Array Command -> String
commandsToNdf cmds = String.joinWith "\n" $ toCode ndf <$> Array.reverse cmds