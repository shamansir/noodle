module Noodle.Raw.Link where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, OutletR, InletR)
import Noodle.Id (Link) as Id


data Link =
    Link
        (Maybe Id.Link)
        { from :: NodeR /\ OutletR, to :: NodeR /\ InletR }
        (Effect Unit)


make :: NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> Link
make nA oA iB nB = Link Nothing { from : nA /\ oA, to : nB /\ iB }


make' :: Id.Link -> NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> Link
make' id nA oA iB nB = Link (Just id) { from : nA /\ oA, to : nB /\ iB }


from :: Link -> NodeR /\ OutletR
from (Link _ { from } _) = from


to :: Link -> NodeR /\ InletR
to (Link _ { to } _) = to


fromNode :: Link -> NodeR
fromNode = from >>> Tuple.fst


toNode :: Link -> NodeR
toNode = to >>> Tuple.fst


id :: Link -> Maybe Id.Link
id (Link id _ _) = id


cancel :: Link -> Effect Unit
cancel (Link _ _ canceller) = canceller


setId :: Id.Link -> Link -> Link
setId id (Link _ rec c) = Link (Just id) rec c