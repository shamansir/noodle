module Cli.Panels where

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
    | Console


-- FIXME: by logic, those are just parts of the `State`, may be make them a direct members and store only toggles here
type SidePanels =
    { commands :: Boolean /\ NdfFile
    , wsServer :: Boolean
    -- , hydraCode :: Boolean
    , documentation :: Boolean /\ Array T.Tag
    , console :: Boolean /\ Array String
    }


initPanels :: SidePanels
initPanels =
    { commands : false /\ initCommands
    , wsServer : false
    -- , hydraCode : false
    , documentation : false /\ []
    , console : false /\ []
    }


initCommands :: NdfFile
initCommands = Ndf.init "noodle" 1.0


insertDocs :: Array T.Tag -> SidePanels -> SidePanels
insertDocs docs s = s { documentation = docs <$ s.documentation }


clearCommands :: SidePanels -> SidePanels
clearCommands s = s { commands = initCommands <$ s.console }


appendCommand :: Ndf.CommandOp -> SidePanels -> SidePanels
appendCommand cmdop s =
    s { commands = Ndf.optimize <$> flip Ndf.snocOp cmdop <$> s.commands }


appendNdf :: NdfFile -> SidePanels -> SidePanels
appendNdf ndfFile s =
    s { commands = Ndf.optimize <$> Ndf.append ndfFile <$> s.commands }


clearLog :: SidePanels -> SidePanels
clearLog s = s { console = [] <$ s.console }


logToConsole :: Array String -> SidePanels -> SidePanels
logToConsole lines s =
    s { console = (flip (<>) lines) <$> s.console }


toggle :: Which -> SidePanels -> SidePanels
toggle w s = case w of
    Commands -> s { commands = lmap not s.commands }
    WsServer -> s { wsServer = not s.wsServer }
    Documentation -> s { documentation = lmap not s.documentation }
    Console -> s { console = lmap not s.console }


load :: Which -> SidePanels -> Boolean /\ Array T.Tag
load w = case w of
    Commands ->      _.commands >>> map (Array.singleton <<< Ndf.toTaggedNdfCode)
    WsServer ->      _.wsServer >>> (flip (/\) [])
    Documentation -> _.documentation
    Console ->       _.console >>> map (map T.s)