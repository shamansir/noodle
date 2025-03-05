-- | All the IDs are managed from here

module Noodle.Id
    ( module FromShape
    , Node, NodeR
    , nodeR, nodeFamily, nodeR_, unsafeNodeR
    , Family(..), FamilyR
    , family, familyR, familyOf, unsafeFamilyR
    , PatchR, PatchName, patchR
    , FnName
    , Link(..)
    , Group(..), GroupR
    , group, groupR, unsafeGroupR
    , ToolkitR
    , toolkit, toolkitR
    , class HasUniqueHash, hashOf
    )
    where


import Prelude

import Type.Proxy (Proxy(..))

import Data.Newtype (class Newtype)
import Data.UniqueHash (UniqueHash)
import Data.UniqueHash (toString) as UH
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Reflectable (class Reflectable)
import Data.Tuple.Nested (type (/\), (/\))


import Noodle.Fn.Shape
    ( Inlet(..), inletName
    , Outlet(..), outletName
    ) as FromShape
import Noodle.Raw.Fn.Shape
    ( InletR, inletR, inletRName, unsafeInletR
    , OutletR, outletR, outletRName, unsafeOutletR
    , InletIndex(..), OutletIndex(..)
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
    show (NodeR { family, hash }) = "<" <> family <> ":" <> UH.toString hash <> ">"


instance Eq NodeR where
    eq (NodeR nodeA) (NodeR nodeB) = nodeA.family == nodeB.family && nodeA.hash == nodeB.hash
instance Ord NodeR where
    compare (NodeR nodeA) (NodeR nodeB) = compare (nodeA.family <> UH.toString nodeA.hash) (nodeB.family <> UH.toString nodeB.hash)


nodeR :: forall family. IsSymbol family => Node family -> NodeR
nodeR (Node { hash }) = NodeR { family : reflectSymbol (Proxy :: _ family), hash }


unsafeNodeR :: FamilyR -> UniqueHash -> NodeR
unsafeNodeR familyR uniqueHash = NodeR { hash : uniqueHash, family : family familyR }


family :: FamilyR -> String
family (FamilyR { family }) = family


familyOf :: NodeR -> FamilyR
familyOf (NodeR { family }) = FamilyR { family }


nodeFamily :: forall f. IsSymbol f => Node f -> String
nodeFamily = const $ reflectSymbol (Proxy :: _ f)


familyR :: forall proxy family. IsSymbol family => proxy family -> FamilyR
familyR _ = unsafeFamilyR $ reflectSymbol (Proxy :: _ family)


unsafeFamilyR :: String -> FamilyR -- FIXME: it is safe to create `FamilyR` for raw nodes definitions, but "unsafe" to create it where `Family f` (i.e. in the context of typed non-raw nodes) is around
unsafeFamilyR family = FamilyR { family }


nodeR_ :: FamilyR -> UniqueHash -> NodeR
nodeR_ (FamilyR { family }) hash = NodeR { family, hash }


type FnName = String


newtype Link = Link -- links are unique by the nodes and channels they connect, even if there are multiple links (many from one outlet or many to one inlet) allowed
    { from :: NodeR /\ FromShape.OutletR
    , to   :: NodeR /\ FromShape.InletR
    }


derive instance Newtype Link _


derive instance Eq Link
derive instance Ord Link
derive newtype instance Show Link


type PatchName = String


newtype PatchR = PatchR { hash :: UniqueHash }


derive instance Eq PatchR
derive instance Ord PatchR
instance Show PatchR where show (PatchR { hash }) = "<Patch::" <> show hash <> ">"


patchR :: UniqueHash -> PatchR
patchR hash = PatchR { hash }


data Group :: Symbol -> Type
data Group g = Group


newtype GroupR = GroupR { group :: String }


instance Show GroupR where
    show (GroupR { group }) = group


instance IsSymbol g => Show (Group g) where
    show _ = reflectSymbol (Proxy :: _ g)


instance IsSymbol g => Reflectable (Group g) GroupR
    where
        reflectType :: Proxy (Group g) -> GroupR
        reflectType _ = GroupR { group : reflectSymbol (Proxy :: _ g) }


derive instance Eq GroupR
derive instance Ord GroupR


groupR :: forall proxy group. IsSymbol group => proxy group -> GroupR
groupR _ = unsafeGroupR $ reflectSymbol (Proxy :: _ group)


unsafeGroupR :: String -> GroupR -- FIXME: it is safe to create `GroupR` for raw nodes definitions, but "unsafe" to create it where `Group g` (i.e. in the context of typed non-raw nodes) is around
unsafeGroupR group = GroupR { group }


group :: GroupR -> String
group (GroupR { group }) = group


newtype ToolkitR = ToolkitR { toolkit :: String }


derive instance Eq ToolkitR
derive instance Ord ToolkitR


toolkitR :: String -> ToolkitR
toolkitR toolkit = ToolkitR { toolkit }


toolkit :: ToolkitR -> String
toolkit (ToolkitR { toolkit }) = toolkit -- unwrap >>> _.toolkit


class HasUniqueHash a where
    hashOf :: a -> UniqueHash


instance HasUniqueHash NodeR where
    hashOf (NodeR { hash }) = hash


instance HasUniqueHash PatchR where
    hashOf (PatchR { hash }) = hash