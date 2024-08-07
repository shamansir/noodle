-- | All the IDs are managed from here

module Noodle.Id where


import Prelude

import Type.Proxy (Proxy(..))
import Data.UniqueHash (UniqueHash)
import Data.Symbol (class IsSymbol, reflectSymbol)



-- | `Temperament` is stored in Inlet and so it can be `Hot` or `Cold`:
-- |
-- | * _Hot_ means that receiving any new data at it triggers the re-computation of the Node function;
-- | * _Cold_ means that receiving any new data just keeps it held there and node function waits for receiving a data from another hot inlet to trigger;
data Temperament
    = Hot
    | Cold


-- | Outlet ID contains its name on a type-level and order inside it.
data Inlet :: Symbol -> Type
data Inlet name = Inlet { order :: Int, temp :: Temperament }


-- | `InletR` stores rawified Inlet ID, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
data InletR = InletR { name :: String, order :: Int, temp :: Temperament }


-- | Outlet ID contains its name on a type-level and order inside it.
data Outlet :: Symbol -> Type
data Outlet name = Outlet { order :: Int }


-- | `OutletR` stores rawified Outlet ID, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
data OutletR = OutletR { name :: String, order :: Int }


-- | Node ID stores node Family name at type-level and Unique Hash of the node at value-level
data Node :: Symbol -> Type
data Node f = Node { hash :: UniqueHash }


-- | `NodeR` stores rawified Node ID, moving all it's type-level data to value-level. As well, can be created right away when one wants to pass type checks when adding nodes.
-- | (this technique is used when we create nodes from parsed files).
data NodeR = NodeR { family :: String, hash :: UniqueHash }


instance IsSymbol name => Show (Inlet name) where
    show :: Inlet name -> String
    show (Inlet _) = reflectSymbol (Proxy :: _ name)


instance IsSymbol name => Show (Outlet name) where
    show :: Outlet name -> String
    show (Outlet _) = reflectSymbol (Proxy :: _ name)


instance Show InletR where
    show (InletR { name }) = name


instance Show OutletR where
    show (OutletR { name }) = name


inletR :: forall name. IsSymbol name => Inlet name -> InletR
inletR (Inlet { order, temp }) = InletR { name : reflectSymbol (Proxy :: _ name), order, temp }


outletR :: forall name. IsSymbol name => Outlet name -> OutletR
outletR (Outlet { order }) = OutletR { name : reflectSymbol (Proxy :: _ name), order }


nodeR :: forall family. IsSymbol family => Node family -> NodeR
nodeR (Node { hash }) = NodeR { family : reflectSymbol (Proxy :: _ family), hash }


derive instance Eq Temperament
derive instance Eq InletR
derive instance Eq OutletR

derive instance Ord Temperament
derive instance Ord InletR
derive instance Ord OutletR