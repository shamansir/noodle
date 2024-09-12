module Test.MyToolkit.Node.Raw.Sum where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Data.Repr (Repr(..))
import Data.Tuple.Nested ((/\))

import Noodle.Id (FamilyR(..), InletR(..), OutletR(..)) as Id
import Noodle.Raw.Node (Node, InletsValues, OutletsValues) as Raw
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (make) as RawShape
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (receive, send) as RawFn
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (make, spawn) as RawFamily

import Test.MyToolkit.Repr (ISRepr)
import Test.MyToolkit.Repr (ISRepr(..)) as ISRepr


shape :: Raw.Shape
shape =
    RawShape.make
        { inlets :
            [ { name : "a", temp : Hot, order : 0 }
            , { name : "b", temp : Hot, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : "sum", order : 0 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues ISRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.InletR "a") (ISRepr.Int 0)
        # Map.insert (Id.InletR "b") (ISRepr.Int 0)


defaultOutlets :: Raw.OutletsValues ISRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.OutletR "sum") (ISRepr.Int 0)


process :: Raw.Process ISRepr ISRepr Effect
process = do
    mbA <- RawFn.receive $ Id.InletR "a"
    mbB <- RawFn.receive $ Id.InletR "b"
    RawFn.send (Id.OutletR "sum") $ Repr $ ISRepr.Int $ case mbA /\ mbB of
        (Repr (ISRepr.Int a) /\ Repr (ISRepr.Int b)) -> a + b
        _ -> 0


node :: Effect (Raw.Node ISRepr Effect)
node =
    RawFamily.spawn family


family :: Raw.Family ISRepr Effect
family =
    RawFamily.make
        (Id.FamilyR { family : "sumR" })
        ISRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
