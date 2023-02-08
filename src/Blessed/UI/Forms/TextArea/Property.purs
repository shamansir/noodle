module Blessed.UI.Forms.TextArea.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, TextArea, textarea)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    ( value :: String
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets TextArea subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter textarea


getterC
    :: forall subj id prop r' state m a
     . C.GetsC TextArea subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC textarea


value
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC TextArea subj id "value" m String
    => NodeKey subj id -> C.Getter state m String
value = getterC (Proxy :: _ "value") CA.string