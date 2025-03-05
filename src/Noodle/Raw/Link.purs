module Noodle.Raw.Link where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, OutletR, InletR)
import Noodle.Id (Link(..)) as Id


data Link =
    Link
        Connector
        (Effect Unit)


type Connector =
    { from :: NodeR /\ OutletR
    , to   :: NodeR /\ InletR
    }


make :: NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> Link
make nA oA iB nB = Link { from : nA /\ oA, to : nB /\ iB }


from :: Link -> NodeR /\ OutletR
from = connector >>> _.from


to :: Link -> NodeR /\ InletR
to = connector >>> _.to


fromNode :: Link -> NodeR
fromNode = from >>> Tuple.fst


toNode :: Link -> NodeR
toNode = to >>> Tuple.fst


fromOutlet :: Link -> OutletR
fromOutlet = from >>> Tuple.snd


toInlet :: Link -> InletR
toInlet = to >>> Tuple.snd


connector :: Link -> Connector
connector (Link con _) = con


id :: Link -> Id.Link
id = connector >>> Id.Link


cancel :: Link -> Effect Unit
cancel (Link _ canceller) = canceller