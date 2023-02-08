module Blessed.UI.Forms.Form.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, Form, form)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( submission :: Json
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets Form subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter form


getterC
    :: forall subj id prop r' state m a
     . C.GetsC Form subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC form


submission
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Form subj id "submission" m Json
    => NodeKey subj id -> C.Getter state m Json
submission = getterC (Proxy :: _ "submission") CA.json
