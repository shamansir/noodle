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
import Blessed.Internal.BlessedSubj (Subject, TextArea)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    ( value :: String
    )


getter
    :: forall subj id sym r' m a
     . Respresents TextArea subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow m a
getter =
    C.getter


value
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents TextArea subj id
    => NodeKey subj id -> C.Getter m String
value = getter (Proxy :: _ "value") CA.string
