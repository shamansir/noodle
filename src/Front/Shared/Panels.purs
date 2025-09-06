module Front.Shared.Panels where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)
import Data.Text.Format (Tag)
import Data.Text.Format as T
import Data.Array (singleton) as Array
import Data.Set (member) as Set
import Data.Set (Set)

import Noodle.Text.NdfFile.Command.Op (CommandOp) as Ndf
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (init, snocOp, toTaggedNdfCode, optimize, append) as Ndf


data Which
    = Commands
    | WSStatus
    | Documentation
    | HydraCode
    | Console
    | Tree
    | NextControls


derive instance Eq Which
derive instance Ord Which


type SidePanelsOnOff = -- FIXME: Change to `Set Which``
    { commands :: Boolean
    , wsServer :: Boolean
    , hydraCode :: Boolean
    , documentation :: Boolean
    , console :: Boolean
    , tree :: Boolean
    , nextControls :: Boolean
    }


initPanelsOnOff :: SidePanelsOnOff
initPanelsOnOff =
    { commands : false
    , wsServer : false
    , hydraCode : false
    , documentation : false
    , console : false
    , tree : false
    , nextControls : false
    }


isOn :: Which -> SidePanelsOnOff -> Boolean
isOn = case _ of
    Commands -> _.commands
    WSStatus -> _.wsServer
    Documentation -> _.documentation
    Console -> _.console
    HydraCode -> _.hydraCode
    Tree -> _.tree
    NextControls -> _.nextControls


isOff :: Which -> SidePanelsOnOff -> Boolean
isOff = not <<< isOn


toggle :: Which -> SidePanelsOnOff -> SidePanelsOnOff
toggle w s = case w of
    Commands -> s { commands = not s.commands }
    WSStatus -> s { wsServer = not s.wsServer }
    Documentation -> s { documentation = not s.documentation }
    Console -> s { console = not s.console }
    HydraCode -> s { hydraCode = not s.hydraCode }
    Tree -> s { tree = not s.tree }
    NextControls -> s { nextControls = not s.nextControls }


toArray :: SidePanelsOnOff -> Array { which :: Which, on :: Boolean }
toArray sps =
    [ { which : Commands,      on : sps.commands }
    , { which : Documentation, on : sps.documentation }
    , { which : HydraCode,     on : sps.hydraCode }
    , { which : Tree,          on : sps.tree }
    , { which : Console,       on : sps.console }
    , { which : WSStatus,      on : sps.wsServer }
    , { which : NextControls,  on : sps.nextControls }
    ]


fromSet :: Set Which -> SidePanelsOnOff
fromSet set =
    { commands :      set # Set.member Commands
    , documentation : set # Set.member Documentation
    , hydraCode :     set # Set.member HydraCode
    , tree :          set # Set.member Tree
    , console :       set # Set.member Console
    , wsServer :      set # Set.member WSStatus
    , nextControls :  set # Set.member NextControls
    }


allPanels :: Array Which
allPanels = [ Commands, Documentation, HydraCode, Tree, Console, WSStatus, NextControls ]
