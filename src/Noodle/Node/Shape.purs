module Noodle.Node.Shape where


import Noodle.Channel.Shape as Channel


import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))


type Inlets d = (String /-> Channel.Shape d)
type Outlets d = (String /-> Channel.Shape d)


type Shape d = Inlets d /\ Outlets d