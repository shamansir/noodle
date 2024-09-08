module Noodle.Raw.Link where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, OutletR, InletR)


data Link =
    Link
        { from :: NodeR /\ OutletR, to :: NodeR /\ InletR }
        (Effect Unit)


make :: NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> Link
make nA oA iB nB = Link { from : nA /\ oA, to : nB /\ iB }
