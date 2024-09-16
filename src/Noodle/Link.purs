module Noodle.Link where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect (Effect)


import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, Node, Inlet, Outlet, InletR, OutletR, nodeR, inletR, outletR)
import Noodle.Id (Link) as Id
import Noodle.Raw.Link (Link(..)) as Raw


type FromId = NodeR /\ OutletR


type ToId = NodeR /\ InletR


data Link (fA :: Symbol) (fB :: Symbol) (oA :: Symbol) (iB :: Symbol) =
    Link
        (Maybe Id.Link)
        { from :: NodeR /\ OutletR, to :: NodeR /\ InletR }
        (Effect Unit)


setId :: forall fA fB oA iB. Id.Link -> Link fA fB oA iB -> Link fA fB oA iB
setId id (Link _ rec c) = Link (Just id) rec c


make :: forall fA fB oA iB. IsSymbol fA => IsSymbol fB => IsSymbol oA => IsSymbol iB => Node fA -> Outlet oA -> Inlet iB -> Node fB -> Effect Unit -> Link fA fB oA iB
make nA oA iB nB = Link Nothing { from : nodeR nA /\ outletR oA, to : nodeR nB /\ inletR iB }


make' :: forall fA fB oA iB. IsSymbol fA => IsSymbol fB => IsSymbol oA => IsSymbol iB => Id.Link -> Node fA -> Outlet oA -> Inlet iB -> Node fB -> Effect Unit -> Link fA fB oA iB
make' linkId nA oA iB nB = Link (Just linkId) { from : nodeR nA /\ outletR oA, to : nodeR nB /\ inletR iB }


toRaw :: forall fA fB oA iB. Link fA fB oA iB -> Raw.Link
toRaw (Link mbId { from, to } cancel) = Raw.Link mbId { from, to } cancel


fromRaw :: forall fA fB oA iB. Raw.Link -> Link fA fB oA iB
fromRaw (Raw.Link mbId { from, to } cancel) = Link mbId { from, to } cancel


from :: forall fA fB oA iB. Link fA fB oA iB -> NodeR /\ OutletR
from (Link _ { from } _) = from


to :: forall fA fB oA iB. Link fA fB oA iB -> NodeR /\ InletR
to (Link _ { to } _) = to


fromNode :: forall fA fB oA iB. Link fA fB oA iB -> NodeR
fromNode = from >>> Tuple.fst


toNode :: forall fA fB oA iB. Link fA fB oA iB -> NodeR
toNode = to >>> Tuple.fst


fromOutlet :: forall fA fB oA iB. Link fA fB oA iB -> OutletR
fromOutlet = from >>> Tuple.snd


toInlet :: forall fA fB oA iB. Link fA fB oA iB -> InletR
toInlet = to >>> Tuple.snd


cancel :: forall fA fB oA iB. Link fA fB oA iB -> Effect Unit
cancel (Link _ _ canceller) = canceller


id :: forall fA fB oA iB. Link fA fB oA iB -> Maybe Id.Link
id (Link mbId _ _) = mbId