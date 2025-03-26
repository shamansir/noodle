module Web.Class.WebRenderer where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\), type (/\))

import Type.Proxy (Proxy)
import Type.Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))

import Control.Monad.State (class MonadState)

import Blessed.Internal.BlessedOp (BlessedOp, BlessedOp')

import Noodle.Id (Family, FamilyR, NodeR, InletR) as Id
import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Toolkit (ToolkitKey)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Cli.Keys (NodeBoxKey)
import Cli.Components.ValueEditor (ValueEditor)


class WebLocator :: Type -> Constraint
class WebLocator x where
    firstLocation :: x
    locateNext :: x -> { width :: Number, height :: Number } -> x /\ { left :: Number, top :: Number }
    -- defaultSize :: Proxy x -> { width :: Int, height :: Int }


data ConstantShift
    = First
    | NextAfter { left :: Number, top :: Number }


instance WebLocator ConstantShift where
    firstLocation :: ConstantShift
    firstLocation = First
    locateNext :: ConstantShift -> { width :: Number, height :: Number } -> ConstantShift /\ { left :: Number, top :: Number }
    locateNext (NextAfter { left, top }) _ =
        let
            nextPos =
                { left : 16.0 + left + 2.0
                , top : top + 2.0
                }
        in NextAfter nextPos /\ nextPos
    locateNext First _ = NextAfter { left : 16.0, top : 0.0 } /\ { left : 16.0, top : 0.0 }
    -- defaultSize :: Proxy PixelShift -> { width :: Int, height :: Int }
    -- defaultSize _ = { width : 5, height : 2 }