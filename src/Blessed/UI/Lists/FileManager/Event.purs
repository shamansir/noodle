module Blessed.UI.Lists.FileManager.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.BlessedSubj (FileManager, class Extends)
import Blessed.Internal.Emitter (class Events, CoreEvent(..), class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Cd
    | File


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Cd = "cd" /\ []
    convert File = "file" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing


instance C.Fires FileManager Event


type Handler subj id r state = C.Handler subj id r state Event


fmHandler :: forall subj id r state. Extends FileManager subj => C.Fires subj Event => Event -> Handler subj id r state
fmHandler = C.handler



on :: forall subj id r state. Extends FileManager subj => C.Fires subj Event => Event -> Handler subj id r state
on = fmHandler