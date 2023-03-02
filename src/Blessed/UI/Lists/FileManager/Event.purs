module Blessed.UI.Lists.FileManager.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.BlessedSubj (FileManager, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = FileManagerEvent


data FileManagerEvent
    = Init
    | Cd
    | File


instance events :: C.Events FileManagerEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Cd = "cd" /\ []
    convert File = "file" /\ []

    uniqueId e = C.defaultUniqueId e


instance C.Fires FileManager FileManagerEvent


fmHandler :: forall subj id r state. Extends FileManager subj => C.Fires subj FileManagerEvent => FileManagerEvent -> C.Handler subj id r state
fmHandler = C.handler



on :: forall subj id r state. Extends FileManager subj => C.Fires subj FileManagerEvent => FileManagerEvent -> C.Handler subj id r state
on = fmHandler