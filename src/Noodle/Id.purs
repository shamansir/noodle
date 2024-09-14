-- | All the IDs are managed from here

module Noodle.Id
    ( module FromShape
    , Node, NodeR
    , nodeR, nodeFamily, nodeR_
    , Family(..), FamilyR(..)
    , family, familyR, familyOf
    , PatchR, PatchName, patchR
    , FnName
    , Link
    )
    where


import Prelude

import Type.Proxy (Proxy(..))
import Data.UniqueHash (UniqueHash)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Reflectable (class Reflectable)



import Noodle.Fn.Shape
    ( Inlet(..), InletR(..), inletR, inletRName
    , Outlet(..), OutletR(..), outletR, outletRName
    ) as FromShape
import Noodle.Fn.Shape.Temperament
    ( Temperament(..)-- , TemperamentK, Hot, Cold)
    ) as FromShape


data Family :: Symbol -> Type
data Family f = Family


newtype FamilyR = FamilyR { family :: String }


instance Show FamilyR where
    show (FamilyR { family }) = family


instance IsSymbol f => Show (Family f) where
    show _ = reflectSymbol (Proxy :: _ f)


instance IsSymbol f => Reflectable (Family f) FamilyR
    where
        reflectType :: Proxy (Family f) -> FamilyR
        reflectType _ = FamilyR { family : reflectSymbol (Proxy :: _ f) }


derive instance Eq FamilyR
derive instance Ord FamilyR


-- | Node ID stores node Family name at type-level and Unique Hash of the node at value-level
data Node :: Symbol -> Type
data Node f = Node { hash :: UniqueHash }


-- | `NodeR` stores rawified Node ID, moving all it's type-level data to value-level. As well, can be created right away when one wants to pass type checks when adding nodes.
-- | (this technique is used when we create nodes from parsed files).
newtype NodeR = NodeR { family :: String, hash :: UniqueHash }


instance Show NodeR where
    show (NodeR { family, hash }) = "<" <> show family <> ":" <> show hash <> ">"


newtype PatchR = PatchR { hash :: UniqueHash }


instance Eq NodeR where
    eq (NodeR nodeA) (NodeR nodeB) = nodeA.family == nodeB.family && nodeA.hash == nodeB.hash


nodeR :: forall family. IsSymbol family => Node family -> NodeR
nodeR (Node { hash }) = NodeR { family : reflectSymbol (Proxy :: _ family), hash }


family :: FamilyR -> String
family (FamilyR { family }) = family


familyOf :: NodeR -> FamilyR
familyOf (NodeR { family }) = FamilyR { family }


nodeFamily :: forall f. IsSymbol f => Node f -> String
nodeFamily = const $ reflectSymbol (Proxy :: _ f)


familyR :: forall proxy family. IsSymbol family => proxy family -> FamilyR
familyR _ = FamilyR { family : reflectSymbol (Proxy :: _ family) }


nodeR_ :: FamilyR -> UniqueHash -> NodeR
nodeR_ (FamilyR { family }) hash = NodeR { family, hash }


type FnName = String


type Link = String


type PatchName = String


patchR :: UniqueHash -> PatchR
patchR hash = PatchR { hash }