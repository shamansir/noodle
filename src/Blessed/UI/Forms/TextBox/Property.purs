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
import Blessed.Internal.BlessedSubj (Subject, TextBox)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)


-- newtype Focused = Focused String


type PropertiesRow =
    ( secret :: String
    , censor :: String
    )


getter
    :: forall subj id sym r' state m a
     . Respresents TextBox subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow state m a
getter =
    C.getter


censor
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents TextBox subj id
    => NodeKey subj id -> C.Getter state m String
censor = getter (Proxy :: _ "censor") CA.string


secret
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents TextBox subj id
    => NodeKey subj id -> C.Getter state m String
secret = getter (Proxy :: _ "secret") CA.string
