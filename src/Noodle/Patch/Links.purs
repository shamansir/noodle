module Noodle.Patch.Links where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Map (Map)
import Data.Map (empty, insert, alter) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (singleton, cons) as Array

import Noodle.Id (Link, NodeR) as Id
import Noodle.Link (FromId, ToId, toRaw, from, to, fromNode, toNode) as Link
import Noodle.Link (Link)
import Noodle.Raw.Link (Link) as Raw


type Links = -- TODO: separate module
  { lastId :: Maybe Id.Link
  , from :: Map Link.FromId Raw.Link
  , to :: Map Link.ToId Raw.Link
  , byNode :: Map Id.NodeR (Array (Link.FromId /\ Link.ToId))
  , byId :: Map Id.Link Raw.Link
  }


init :: Links
init =
  { lastId : Nothing
  , from : Map.empty
  , to : Map.empty
  , byNode : Map.empty
  , byId : Map.empty
  }


track :: forall fA fB oA iB. Link fA fB oA iB -> Links -> Links
track link { lastId, from, to, byNode, byId } =
  let
    rawLink = Link.toRaw link
    nextId = maybe 0 ((+) 1) lastId
    alterF tpl = maybe (Just $ Array.singleton tpl) (Array.cons tpl >>> Just)
  in
    { lastId : Just nextId
    , from : from # Map.insert (Link.from link) rawLink
    , to : to # Map.insert (Link.to link) rawLink
    , byNode :
        byNode
          # Map.alter (alterF (Link.from link /\ Link.to link)) (Link.fromNode link)
          # Map.alter (alterF (Link.from link /\ Link.to link)) (Link.toNode link)
    , byId :
        byId #
          Map.insert nextId rawLink
    }


-- TODO: trackRaw :: Raw.Link -> Links -> Links
-- TODO: forget :: forall fA fB oA iB. Link fA fB oA iB -> Links -> Links
-- TODO: forgetRaw :: Raw.Link -> Links -> Links
