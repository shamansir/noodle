module Blessed.UI.Forms.Checkbox.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, Checkbox, checkbox)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( text :: String
    , checked :: Boolean
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets Checkbox subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter checkbox


getterC
    :: forall subj id prop r' state m a
     . C.GetsC Checkbox subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC checkbox


text
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Checkbox subj id "text" m String
    => NodeKey subj id -> C.Getter state m String
text = getterC (Proxy :: _ "text") CA.string


checked
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Checkbox subj id "checked" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
checked = getterC (Proxy :: _ "checked") CA.boolean