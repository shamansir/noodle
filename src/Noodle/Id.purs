-- | All the IDs are managed from here

module Noodle.Id
    ( module FromShape
    , Node, NodeR
    , nodeR, nodeFamily
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



-- | Node ID stores node Family name at type-level and Unique Hash of the node at value-level
data Node :: Symbol -> Type
data Node f = Node { hash :: UniqueHash }


-- | `NodeR` stores rawified Node ID, moving all it's type-level data to value-level. As well, can be created right away when one wants to pass type checks when adding nodes.
-- | (this technique is used when we create nodes from parsed files).
newtype NodeR = NodeR { family :: String, hash :: UniqueHash }


nodeR :: forall family. IsSymbol family => Node family -> NodeR
nodeR (Node { hash }) = NodeR { family : reflectSymbol (Proxy :: _ family), hash }


nodeFamily :: forall f. IsSymbol f => Node f -> String
nodeFamily = const $ reflectSymbol (Proxy :: _ f)