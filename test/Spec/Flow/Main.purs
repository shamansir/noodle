module Noodle.Test.Spec.Flow
    ( spec ) where


import Prelude (Unit, ($), discard)
import Test.Spec (Spec, describe)

import Noodle.Test.Spec.Flow.Inlets (spec) as Inlets
import Noodle.Test.Spec.Flow.Outlets (spec) as Outlets
import Noodle.Test.Spec.Flow.Links (spec) as Links
import Noodle.Test.Spec.Flow.Nodes (spec) as Nodes
import Noodle.Test.Spec.Flow.Network (spec) as Network
import Noodle.Test.Spec.Flow.Subscriptions (spec) as Subscriptions
import Noodle.Test.Spec.Flow.Subpatches (spec) as Subpatches


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

    describe "for subpatches"
      Subpatches.spec

    describe "on subscriptions"
      Subscriptions.spec
