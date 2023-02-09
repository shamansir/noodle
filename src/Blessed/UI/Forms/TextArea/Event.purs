module Blessed.UI.Forms.TextArea.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.BlessedSubj (TextArea, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires) as C
import Blessed.Internal.Core (handler, Handler) as C


type Event = TextAreaEvent


data TextAreaEvent
    = Init
    | Submit
    | Cancel
    | Action


instance events :: C.Events TextAreaEvent where
    initial = Init

    convert Init = "init" /\ []
    convert Submit = "submit" /\ []
    convert Cancel = "cancel" /\ []
    convert Action = "action" /\ []


instance C.Fires TextArea TextAreaEvent


type Handler subj id r state = C.Handler subj id r state TextAreaEvent


textAreaHandler :: forall subj id r state. Extends TextArea subj => C.Fires subj TextAreaEvent => TextAreaEvent -> Handler subj id r state
textAreaHandler = C.handler


on :: forall subj id r state. Extends TextArea subj => C.Fires subj TextAreaEvent => TextAreaEvent -> Handler subj id r state
on = textAreaHandler