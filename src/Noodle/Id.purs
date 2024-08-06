-- | All the IDs are managed from here

module Noodle.Id where


import Prelude

import Type.Proxy (Proxy(..))
import Data.UniqueHash (UniqueHash)
import Data.Symbol (class IsSymbol, reflectSymbol)



data Temperament
    = Hot
    | Cold


-- | Inlet ID contains its name and position on a type-level and that allows to ensure connecting to existing inlets, like order in a node.
-- |
-- | It is expected that you reuse the stored symbols defined in the node.
-- |
-- | Inlet can be `Hot` or `Cold`:
-- |
-- | * _Hot_ means that receiving any new data at it triggers the re-computation of the Node function.
-- | * _Cold_ means that receiving any new data just keeps it held there and node function waits for receiving a data from anther hot inlet to trigger.
-- |
-- | So in most cases it makes sense to have minimum one `Hot` inlet in the `Node``. However, you may trigger the function some other way.
data Inlet :: Symbol -> Int -> Type
data Inlet name pos = Inlet Temperament


-- | `InletR` stores rawified Inlet ID, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
data InletR = InletR { name :: String, order :: Int, temp :: Temperament }


-- | Outlet ID contains its name and position on a type-level and that allows to ensure connecting from existing outlets and many other thigs, like order in a node.
data Outlet :: Symbol -> Int -> Type
data Outlet name pos = Outlet Temperament


-- | `OutletR` stores rawified Outlet ID, moving all it's type-level data to value-level. Or, it can be created right away for the cases where it safe to be unsafe.
data OutletR = OutletR { name :: String, order :: Int }


-- | Node ID stores node Family name at type-level and Unique Hash of the node at value-level
data Node :: Symbol -> Type
data Node f = Node UniqueHash


-- | `NodeR` stores rawified Node ID, moving all it's type-level data to value-level. As well, can be created right away when one wants to pass type checks when adding nodes.
-- | (this technique is used when we create nodes from parsed files).
data NodeR = NodeR { family :: String, hash :: String }


instance IsSymbol name => Show (Inlet name pos) where
    show :: Inlet name pos -> String
    show (Inlet _) = reflectSymbol (Proxy :: _ name)


instance IsSymbol name => Show (Outlet name pos) where
    show :: Outlet name pos -> String
    show (Outlet _) = reflectSymbol (Proxy :: _ name)


instance Show InletR where
    show (InletR { name }) = name


instance Show OutletR where
    show (OutletR { name }) = name