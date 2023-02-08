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
import Blessed.Internal.BlessedSubj (Subject, FileManager, filemanager)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C


-- newtype Focused = Focused String


type PropertiesRow =
    ( cwd :: String
    ) -- + ListOption


getter
    :: forall subj id prop r' state m a
     . C.Gets FileManager subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter filemanager


getterC
    :: forall subj id prop r' state m a
     . C.GetsC FileManager subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC filemanager


cwd
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC FileManager subj id "cwd" m String
    => NodeKey subj id -> C.Getter state m String
cwd = getterC (Proxy :: _ "cwd") CA.string
