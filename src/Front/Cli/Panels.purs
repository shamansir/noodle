module Cli.Panels where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))

import Data.Text.Format (Tag)
import Data.Text.Format as T

import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, snocOp) as Ndf


type SidePanels = -- panels state
    { commands :: Boolean /\ NdfFile
    , wsServer :: Boolean
    -- , hydraCode :: Boolean
    , documentation :: Boolean /\ Array T.Tag
    , console :: Boolean /\ Array String
    }


initPanels :: SidePanels
initPanels =
    { commands : false /\ Ndf.init "noodle" 1.0
    , wsServer : false
    -- , hydraCode : false
    , documentation : false /\ []
    , console : false /\ []
    }


insertDocs :: Array T.Tag -> SidePanels -> SidePanels
insertDocs docs s = s { documentation = docs <$ s.documentation }


clearCommands :: SidePanels -> SidePanels
clearCommands s = s { commands = Ndf.init "noodle" 1.0 <$ s.console }


appendCommand :: Ndf.CommandOp -> SidePanels -> SidePanels
appendCommand cmdop s =
    s { commands = flip Ndf.snocOp cmdop <$> s.commands }


clearLog :: SidePanels -> SidePanels
clearLog s = s { console = [] <$ s.console }


logToConsole :: Array String -> SidePanels -> SidePanels
logToConsole lines s =
    s { console = (flip (<>) lines) <$> s.console }