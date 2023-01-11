module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)

import Effect (Effect)
import Effect.Console (log)

import Noodle.Network2 (Network)
import Noodle.Network2 as Network
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Patch4 as Patch
import Noodle.Id (Family(..)) as Node
import Noodle.Id (reflect')

import Toolkit.Test (toolkit)

_foo = (Node.Family :: Node.Family "foo")
_bar = (Node.Family :: Node.Family "foo")


app gstate nw =
  { toolkit
  -- , components
  , currentPatch : Nothing -- Just "hydra"
  , network : nw
  , patchState : gstate
  }


main :: Effect Unit
main = do
  nodeA <- Toolkit.spawn toolkit _foo
  nodeB <- Toolkit.spawn toolkit _bar
  nodeC <- Toolkit.spawn toolkit _bar

  let
    patch = Patch.init toolkit
                # Patch.registerNode nodeA
                # Patch.registerNode nodeB
                # Patch.registerNode nodeC
    nw = Network.init toolkit
                # Network.addPatch "test" patch
    state = app unit nw

  traverse_ log (reflect' <$> Toolkit.nodeFamilies toolkit)

  log "🍝"
