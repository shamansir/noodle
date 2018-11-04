module Rpd.Command where

import Prelude (class Eq, class Show, show, (<>))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow

import Rpd.Path as P

data Command
    = Bang
    | AddPatch String
    | AddNode P.PatchId String
    | AddInlet P.NodePath String
    | AddOutlet P.NodePath String
    | Connect P.OutletPath P.InletPath
    | Disconnect P.OutletPath P.InletPath
    -- | DeleteNode
    -- | DeleteInlet
    -- | DeleteOutlet

derive instance genericCommand :: Generic Command _
instance eqCommand :: Eq Command where
  eq = GEq.genericEq
instance showCommand :: Show Command where
  show = GShow.genericShow
