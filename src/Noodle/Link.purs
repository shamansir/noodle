module Noodle.Link where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect (Effect)

import Noodle.Id (Family, NodeR, Node, Inlet, Outlet, nodeR, familyR, inletR, outletR)

import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, OutletR, InletR, Inlet, Outlet)


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