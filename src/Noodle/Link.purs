module Noodle.Link where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect (Effect)

import Noodle.Id (Family, NodeR, Node, Inlet, Outlet, nodeR, familyR, inletR, outletR)

import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, OutletR, InletR, Inlet, Outlet)


type Id = String


type FromId = NodeR /\ OutletR


type ToId = NodeR /\ InletR


data Link (fA :: Symbol) (fB :: Symbol) (oA :: Symbol) (iB :: Symbol) =
    Link
        { from :: NodeR /\ OutletR, to :: NodeR /\ InletR }
        (Effect Unit)


data RawLink =
    RawLink
        { from :: NodeR /\ OutletR, to :: NodeR /\ InletR }
        (Effect Unit)


make :: forall fA fB oA iB. IsSymbol fA => IsSymbol fB => IsSymbol oA => IsSymbol iB => Node fA -> Outlet oA -> Inlet iB -> Node fB -> Effect Unit -> Link fA fB oA iB
make nA oA iB nB = Link { from : nodeR nA /\ outletR oA, to : nodeR nB /\ inletR iB }


makeRaw :: NodeR -> OutletR -> InletR -> NodeR -> Effect Unit -> RawLink
makeRaw nA oA iB nB = RawLink { from : nA /\ oA, to : nB /\ iB }


toRaw :: forall fA fB oA iB. Link fA fB oA iB -> RawLink
toRaw (Link { from, to } cancel) = RawLink { from, to } cancel



fromRaw :: forall fA fB oA iB. RawLink -> Link fA fB oA iB
fromRaw (RawLink { from, to } cancel) = Link { from, to } cancel


from :: forall fA fB oA iB. Link fA fB oA iB -> NodeR /\ OutletR
from (Link { from } _) = from


to :: forall fA fB oA iB. Link fA fB oA iB -> NodeR /\ InletR
to (Link { to } _) = to


fromNode :: forall fA fB oA iB. Link fA fB oA iB -> NodeR
fromNode = from >>> Tuple.fst


toNode :: forall fA fB oA iB. Link fA fB oA iB -> NodeR
toNode = to >>> Tuple.fst


fromOutlet :: forall fA fB oA iB. Link fA fB oA iB -> OutletR
fromOutlet = from >>> Tuple.snd


toInlet :: forall fA fB oA iB. Link fA fB oA iB -> InletR
toInlet = to >>> Tuple.snd


cancel :: forall fA fB oA iB. Link fA fB oA iB -> Effect Unit
cancel (Link _ canceller) = canceller