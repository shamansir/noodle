module Example.Toolkit.Minimal.PatchState where

import Prelude

import Data.Newtype (class Newtype)
import Noodle.Repr (class HasFallback)


newtype State =
    State
        { intVal :: Int
        , strVal :: String
        }

derive newtype instance Eq State
derive newtype instance Show State
derive instance Newtype State _


default :: State
default =
    State
        { intVal : 0
        , strVal : ""
        }


instance HasFallback State where fallback = default