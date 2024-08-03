module Blessed.UI.Forms.TextBox.Event where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.BlessedSubj (TextBox, class Extends)
import Blessed.Internal.Emitter (class Events, class Fires, defaultUniqueId) as C
import Blessed.Internal.Core (handler, Handler) as C
import Blessed.UI.Forms.TextArea.Event (Event) as TextArea


type Event = TextBoxEvent


type TextBoxEvent = TextArea.Event


textBoxHandler :: forall subj id r state. Extends TextBox subj => C.Fires subj TextBoxEvent => TextBoxEvent -> C.Handler subj id r state
textBoxHandler = C.handler


on :: forall subj id r state. Extends TextBox subj => C.Fires subj TextBoxEvent => TextBoxEvent -> C.Handler subj id r state
on = textBoxHandler