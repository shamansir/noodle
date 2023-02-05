module Blessed.UI.Lists.FileManager.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, FileManager)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C


-- newtype Focused = Focused String


type PropertiesRow =
    ( cwd :: String
    ) -- + ListOption


getter
    :: forall subj id sym r' state m a
     . Respresents FileManager subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow state m a
getter =
    C.getter


cwd
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents FileManager subj id
    => NodeKey subj id -> C.Getter state m String
cwd = getter (Proxy :: _ "cwd") CA.string
