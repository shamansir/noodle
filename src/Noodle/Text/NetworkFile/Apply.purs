module Noodle.Text.NetworkFile.Apply where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Traversable (traverse)
import Data.Foldable (foldr)


import Noodle.Network2 (Network)
import Noodle.Network2 as NW

import Noodle.Text.NetworkFile.Command (Command)


-- TODO: use NoodleM

-- applyFile :: forall m gstate (nodes :: Row Type) (instances :: Row Type). MonadEffect m => Array Command -> Network gstate nodes instances -> m (Network gstate nodes instances)
applyFile :: forall m gstate (nodes :: Row Type) (instances :: Row Type). MonadEffect m => Network gstate nodes instances -> Array Command -> Array (Network gstate nodes instances -> m (Network gstate nodes instances))
applyFile _ =
    map applyCommand
    where applyCommand cmd nw = pure nw