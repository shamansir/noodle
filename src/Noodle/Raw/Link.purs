module Noodle.Raw.Link where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, OutletR, InletR)
import Noodle.Id (Link) as Id


data Link =
    Link
        (Maybe Id.Link)
        Connector
        (Effect Unit)


type Connector =
    { from :: NodeR /\ OutletR
    , to   :: NodeR /\ InletR
    }


make :: NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> Link
make nA oA iB nB = Link Nothing { from : nA /\ oA, to : nB /\ iB }


make' :: Id.Link -> NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> Link
make' id nA oA iB nB = Link (Just id) { from : nA /\ oA, to : nB /\ iB }


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
connector (Link _ con _) = con


id :: Link -> Maybe Id.Link
id (Link id _ _) = id


cancel :: Link -> Effect Unit
cancel (Link _ _ canceller) = canceller


setId :: Id.Link -> Link -> Link
setId id (Link _ rec c) = Link (Just id) rec c