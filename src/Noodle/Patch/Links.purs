module Noodle.Patch.Links where

import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map (Map)
import Data.Map (empty, insert, alter, delete, lookup, values, filter, filterKeys) as Map
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (singleton, cons, catMaybes, fromFoldable, length) as Array
import Data.Foldable (foldr)
import Data.Newtype (unwrap, wrap) as NT

import Noodle.Id (LinkR(..), NodeR, InletR, OutletR) as Id
import Noodle.Link (FromId, ToId, toRaw) as Link
import Noodle.Link (Link)
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id, from, to, fromNode, toNode) as RawLink


-- `Id.Link` is a combination of `NodeR /\ OutletR` + `NodeR /\ InletR` and so it should be unique for the whole patch
type Links = Map Id.LinkR Raw.Link


{- type Links =
  { from :: Map Link.FromId Raw.Link
  , to :: Map Link.ToId Raw.Link
  , byNode :: Map Id.NodeR (Array (Link.FromId /\ Link.ToId))
  , byId :: Map Id.Link Raw.Link
  }


-}


init :: Links
init = Map.empty


track :: forall fA fB oA iB. Link fA fB oA iB -> Links -> Links
track = trackRaw <<< Link.toRaw


trackRaw :: Raw.Link -> Links -> Links
trackRaw rawLink = Map.insert (RawLink.id rawLink) rawLink


forget :: forall fA fB oA iB. Link fA fB oA iB -> Links -> Links
forget = forgetRaw <<< Link.toRaw


forgetRaw :: Raw.Link -> Links -> Links
forgetRaw rawLink = Map.delete $ RawLink.id rawLink


all :: Links -> Array Raw.Link
all = Map.values >>> Array.fromFoldable


findRaw :: Id.LinkR -> Links -> Maybe Raw.Link
findRaw = Map.lookup


findAllFrom :: Id.NodeR -> Links -> Array Raw.Link
findAllFrom nodeR =
  Map.filterKeys (NT.unwrap >>> _.from >>> Tuple.fst >>> (_ == nodeR))
    >>> Map.values >>> Array.fromFoldable


findAllTo :: Id.NodeR -> Links -> Array Raw.Link
findAllTo nodeR =
  Map.filterKeys (NT.unwrap >>> _.to >>> Tuple.fst >>> (_ == nodeR))
    >>> Map.values >>> Array.fromFoldable


forgetAllFrom :: Id.NodeR -> Links -> (Links /\ Array Raw.Link)
forgetAllFrom nodeR links =
  let (allFrom :: Array Raw.Link) = links # findAllFrom nodeR
  in (allFrom # foldr forgetRaw links) /\ allFrom


forgetAllTo :: Id.NodeR -> Links -> (Links /\ Array Raw.Link)
forgetAllTo nodeR links =
  let (allTo :: Array Raw.Link) = links # findAllTo nodeR
  in (allTo # foldr forgetRaw links) /\ allTo


findFrom :: Id.NodeR -> Id.OutletR -> Links -> Array Raw.Link
findFrom nodeR outletR =
  Map.filterKeys (NT.unwrap >>> _.from >>> (_ == nodeR /\ outletR)) >>> Map.values >>> Array.fromFoldable


findTo :: Id.NodeR -> Id.InletR -> Links -> Array Raw.Link
findTo nodeR inletR =
  Map.filterKeys (NT.unwrap >>> _.to >>> (_ == nodeR /\ inletR)) >>> Map.values >>> Array.fromFoldable


findBetween :: Id.NodeR /\ Id.OutletR -> Id.NodeR /\ Id.InletR -> Links -> Array Raw.Link
findBetween start end = Map.filterKeys (NT.unwrap >>> betweenTheSame) >>> Map.values >>> Array.fromFoldable
  where betweenTheSame { from, to } = from == start && to == end