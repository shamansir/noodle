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
import Blessed.Internal.BlessedSubj (Subject, Checkbox)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( text :: String
    , checked :: Boolean
    )


getter
    :: forall subj id sym r' m a
     . Respresents Checkbox subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow m a
getter =
    C.getter


text
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Checkbox subj id
    => NodeKey subj id -> C.Getter m String
text = getter (Proxy :: _ "text") CA.string


checked
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Checkbox subj id
    => NodeKey subj id -> C.Getter m Boolean
checked = getter (Proxy :: _ "checked") CA.boolean
