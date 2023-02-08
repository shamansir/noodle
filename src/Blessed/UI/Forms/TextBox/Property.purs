module Blessed.UI.Forms.TextBox.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, TextBox, textbox)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    ( secret :: String
    , censor :: String
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets TextBox subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter textbox


getterC
    :: forall subj id prop r' state m a
     . C.GetsC TextBox subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC textbox


censor
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC TextBox subj id "censor" m String
    => NodeKey subj id -> C.Getter state m String
censor = getterC (Proxy :: _ "censor") CA.string


secret
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC TextBox subj id "secret" m String
    => NodeKey subj id -> C.Getter state m String
secret = getterC (Proxy :: _ "secret") CA.string
