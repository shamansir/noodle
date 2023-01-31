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
import Blessed.Internal.BlessedSubj (Subject, Form)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( submission :: Json
    )


getter
    :: forall subj id sym r' m a
     . Respresents Form subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow m a
getter =
    C.getter


submission
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Form subj id
    => NodeKey subj id -> C.Getter m Json
submission = getter (Proxy :: _ "submission") CA.json
