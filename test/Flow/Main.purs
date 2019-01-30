module RpdTest.Flow
    ( spec ) where


import Prelude (Unit, ($), discard)
import Test.Spec (Spec, describe)

import RpdTest.Flow.Inlets (spec) as Inlets
import RpdTest.Flow.Outlets (spec) as Outlets
import RpdTest.Flow.Links (spec) as Links
import RpdTest.Flow.Nodes (spec) as Nodes
import RpdTest.Flow.Network (spec) as Network
import RpdTest.Flow.Subscriptions (spec) as Subscriptions


spec :: Spec Unit
spec = do
  describe "data flow is functioning as expected" $ do

    describe "for inlets"
      Inlets.spec

    describe "for outlets"
      Outlets.spec

    describe "for links between outlets and nodes"
      Links.spec

    describe "for nodes"
      Nodes.spec

    describe "for network"
      Network.spec

    describe "on subscriptions"
      Subscriptions.spec
