module Front.Shared.Panels where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)
import Data.Text.Format (Tag)
import Data.Text.Format as T
import Data.Array (singleton) as Array

import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, snocOp, toTaggedNdfCode, optimize, append) as Ndf


data Which
    = Commands
    | WsServer
    | Documentation
    | HydraCode
    | Console
    | Tree


derive instance Eq Which
derive instance Ord Which


-- FIXME: by logic, those are just parts of the `State`, may be make them a direct members and store only toggles here
type SidePanelsOnOff =
    { commands :: Boolean
    , wsServer :: Boolean
    , hydraCode :: Boolean
    , documentation :: Boolean
    , console :: Boolean
    , tree :: Boolean
    }


initPanelsOnOff :: SidePanelsOnOff
initPanelsOnOff =
    { commands : false
    , wsServer : false
    , hydraCode : false
    , documentation : false
    , console : false
    , tree : false
    }


isOn :: Which -> SidePanelsOnOff -> Boolean
isOn = case _ of
    Commands -> _.commands
    WsServer -> _.wsServer
    Documentation -> _.documentation
    Console -> _.console
    HydraCode -> _.hydraCode
    Tree -> _.tree


isOff :: Which -> SidePanelsOnOff -> Boolean
isOff = not <<< isOn


toggle :: Which -> SidePanelsOnOff -> SidePanelsOnOff
toggle w s = case w of
    Commands -> s { commands = not s.commands }
    WsServer -> s { wsServer = not s.wsServer }
    Documentation -> s { documentation = not s.documentation }
    Console -> s { console = not s.console }
    HydraCode -> s { hydraCode = not s.hydraCode }
    Tree -> s { tree = not s.tree }


allPanels :: Array Which
allPanels = [ Commands, Documentation, HydraCode, Tree, Console ]