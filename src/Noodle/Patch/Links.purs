module Noodle.Patch.Links where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map (Map)
import Data.Map (empty, insert, alter, delete, lookup, values, filterKeys) as Map
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (singleton, cons, catMaybes, fromFoldable, length) as Array
import Data.Foldable (foldr)

import Noodle.Id (Link(..), NodeR) as Id
import Noodle.Link (FromId, ToId, toRaw) as Link
import Noodle.Link (Link)
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id, from, to, fromNode, toNode) as RawLink


-- TODO: get rid of this, now it definitely just stores the same information in different shapes
type Links =
  { from :: Map Link.FromId Raw.Link
  , to :: Map Link.ToId Raw.Link
  , byNode :: Map Id.NodeR (Array (Link.FromId /\ Link.ToId))
  , byId :: Map Id.Link Raw.Link
  }


init :: Links
init =
  { from : Map.empty
  , to : Map.empty
  , byNode : Map.empty
  , byId : Map.empty
  }


track :: forall fA fB oA iB. Link fA fB oA iB -> Links -> Links
track = trackRaw <<< Link.toRaw


trackRaw :: Raw.Link -> Links -> Links
trackRaw rawLink { from, to, byNode, byId } =
  let
    alterF tpl = maybe (Just $ Array.singleton tpl) (Array.cons tpl >>> Just)
  in
    { from : from # Map.insert (RawLink.from rawLink) rawLink
    , to : to # Map.insert (RawLink.to rawLink) rawLink
    , byNode :
        byNode
          # Map.alter (alterF (RawLink.from rawLink /\ RawLink.to rawLink)) (RawLink.fromNode rawLink)
          # Map.alter (alterF (RawLink.from rawLink /\ RawLink.to rawLink)) (RawLink.toNode rawLink)
    , byId :
        byId # Map.insert (RawLink.id rawLink) rawLink
    }


forget :: forall fA fB oA iB. Link fA fB oA iB -> Links -> Links
forget = forgetRaw <<< Link.toRaw


forgetRaw :: Raw.Link -> Links -> Links
forgetRaw rawLink { from, to, byNode, byId } =
    { from : from # Map.delete (RawLink.from rawLink)
    , to : to # Map.delete (RawLink.to rawLink)
    , byNode :
        byNode
          # Map.delete (RawLink.fromNode rawLink)
          # Map.delete (RawLink.toNode rawLink)
    , byId : byId # Map.delete (RawLink.id rawLink)
    }


all :: Links -> Array Raw.Link
all = _.byId >>> Map.values >>> Array.fromFoldable


findRaw :: Id.Link -> Links -> Maybe Raw.Link
findRaw linkId =
  _.byId >>> Map.lookup linkId


findAllFrom :: Id.NodeR -> Links -> Array Raw.Link
findAllFrom nodeR links =
  links.from
    # Map.filterKeys (Tuple.fst >>> (_ == nodeR))
    # Map.values # Array.fromFoldable


findAllTo :: Id.NodeR -> Links -> Array Raw.Link
findAllTo nodeR links =
  links.to
    # Map.filterKeys (Tuple.fst >>> (_ == nodeR))
    # Map.values # Array.fromFoldable


forgetAllFrom :: Id.NodeR -> Links -> (Links /\ Array Raw.Link)
forgetAllFrom nodeR links =
  let
    (allFrom :: Array Raw.Link) = links # findAllFrom nodeR
    -- _ = Debug.spy "count from" $ Array.length allFrom
  in (allFrom # foldr forgetRaw links) /\ allFrom


forgetAllTo :: Id.NodeR -> Links -> (Links /\ Array Raw.Link)
forgetAllTo nodeR links =
  let
    (allTo :: Array Raw.Link) = links # findAllTo nodeR
    -- _ = Debug.spy "count to" $ Array.length allTo
  in (allTo # foldr forgetRaw links) /\ allTo