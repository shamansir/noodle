module Rpd.Test.Spec.Flow
    ( spec ) where


import Prelude (Unit, ($), discard)
import Test.Spec (Spec, describe)

import Rpd.Test.Spec.Flow.Inlets (spec) as Inlets
import Rpd.Test.Spec.Flow.Outlets (spec) as Outlets
import Rpd.Test.Spec.Flow.Links (spec) as Links
import Rpd.Test.Spec.Flow.Nodes (spec) as Nodes
import Rpd.Test.Spec.Flow.Network (spec) as Network
import Rpd.Test.Spec.Flow.Subscriptions (spec) as Subscriptions


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
