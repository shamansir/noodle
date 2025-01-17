module Noodle.Repr.Tag where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Newtype (class Newtype)

import Noodle.Id as Id


data Path
    = Inlet Id.NodeR Id.InletR
    | Outlet Id.NodeR Id.OutletR


newtype Tag = Tag String

derive instance Newtype Tag _
derive newtype instance Eq Tag
derive newtype instance Ord Tag
instance Show Tag where show (Tag tag) = "<" <> tag <> ">"


class Tagged a where
    tag :: Path -> a -> Tag


{-
class TypeTagged a where
    typeTag :: Proxy a -> Tag
-}


use :: String -> Tag
use = Tag


inlet :: Id.NodeR -> Id.InletR -> Path
inlet = Inlet


outlet :: Id.NodeR -> Id.OutletR -> Path
outlet = Outlet