module Noodle.Link where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect (Effect)


import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Id (NodeR, Node, Inlet, Outlet, InletR, OutletR, nodeR, inletR, outletR)
import Noodle.Id (LinkR(..)) as Id
import Noodle.Raw.Link (Link(..), Connector) as Raw


type FromId = NodeR /\ OutletR


type ToId = NodeR /\ InletR


data Link (fA :: Symbol) (fB :: Symbol) (oA :: Symbol) (iB :: Symbol) =
    Link
        Raw.Connector
        (Effect Unit)


make :: forall fA fB oA iB. IsSymbol fA => IsSymbol fB => IsSymbol oA => IsSymbol iB => Node fA -> Outlet oA -> Inlet iB -> Node fB -> Effect Unit -> Link fA fB oA iB
make nA oA iB nB = Link { from : nodeR nA /\ outletR oA, to : nodeR nB /\ inletR iB }


toRaw :: forall fA fB oA iB. Link fA fB oA iB -> Raw.Link
toRaw (Link { from, to } cancel) = Raw.Link { from, to } cancel


fromRaw :: forall fA fB oA iB. Raw.Link -> Link fA fB oA iB
fromRaw (Raw.Link { from, to } cancel) = Link { from, to } cancel


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


id :: forall fA fB oA iB. Link fA fB oA iB -> Id.LinkR
id (Link con _) = Id.LinkR con