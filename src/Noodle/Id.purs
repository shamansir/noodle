-- | All the IDs are managed from here

module Noodle.Id
    ( module FromShape
    , Node, NodeR
    , nodeR, nodeFamily, nodeRaw
    , Family(..), FamilyR
    , family, familyR
    , PatchR
    )
    where



import Prelude

import Type.Proxy (Proxy(..))
import Data.UniqueHash (UniqueHash)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Newtype (class Newtype, unwrap)



import Noodle.Fn.Shape
    ( Temperament(..)-- , TemperamentK, Hot, Cold
    , Inlet(..), InletR, inletR, inletRName
    , Outlet(..), OutletR, outletR, outletRName
    ) as FromShape


data Family :: Symbol -> Type
data Family f = Family


newtype FamilyR = FamilyR { family :: String }


-- | Node ID stores node Family name at type-level and Unique Hash of the node at value-level
data Node :: Symbol -> Type
data Node f = Node { hash :: UniqueHash }


-- | `NodeR` stores rawified Node ID, moving all it's type-level data to value-level. As well, can be created right away when one wants to pass type checks when adding nodes.
-- | (this technique is used when we create nodes from parsed files).
newtype NodeR = NodeR { family :: String, hash :: UniqueHash }


newtype PatchR = PatchR { hash :: UniqueHash }


instance Eq NodeR where
    eq (NodeR nodeA) (NodeR nodeB) = nodeA.family == nodeB.family && nodeA.hash == nodeB.hash


nodeR :: forall family. IsSymbol family => Node family -> NodeR
nodeR (Node { hash }) = NodeR { family : reflectSymbol (Proxy :: _ family), hash }


family :: FamilyR -> String
family (FamilyR { family }) = family


nodeFamily :: forall f. IsSymbol f => Node f -> String
nodeFamily = const $ reflectSymbol (Proxy :: _ f)


familyR :: forall family. IsSymbol family => Family family -> FamilyR
familyR Family = FamilyR { family : reflectSymbol (Proxy :: _ family) }


nodeRaw :: FamilyR -> UniqueHash -> NodeR
nodeRaw (FamilyR { family }) hash = NodeR { family, hash }