module Example.Toolkit.Minimal.Node.Raw.Sum where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Noodle.Repr.ChRepr (ChRepr(..))
import Data.Tuple.Nested ((/\))

import Noodle.Raw.Id (inletR, outletR, familyR) as Id
import Noodle.Raw.Node (Node, InletsValues, OutletsValues) as Raw
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (make) as RawShape
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (receive, send) as RawFn
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (make, spawn) as RawFamily

import Example.Toolkit.Minimal.Repr (MinimalRepr)
import Example.Toolkit.Minimal.Repr (MinimalRepr(..)) as MinimalRepr


shape :: Raw.Shape
shape =
    RawShape.make
        { inlets :
            [ { name : Id.inletR "a", temp : Hot, order : 0 }
            , { name : Id.inletR "b", temp : Hot, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "sum", order : 0 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues MinimalRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.inletR "a") (MinimalRepr.Int 0)
        # Map.insert (Id.inletR "b") (MinimalRepr.Int 0)


defaultOutlets :: Raw.OutletsValues MinimalRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.outletR "sum") (MinimalRepr.Int 0)


process :: Raw.Process MinimalRepr MinimalRepr Effect
process = do
    mbA <- RawFn.receive $ Id.inletR "a"
    mbB <- RawFn.receive $ Id.inletR "b"
    RawFn.send (Id.outletR "sum") $ ChRepr $ MinimalRepr.Int $ case mbA /\ mbB of
        (ChRepr (MinimalRepr.Int a) /\ ChRepr (MinimalRepr.Int b)) -> a + b
        _ -> 0


makeNode :: Effect (Raw.Node MinimalRepr MinimalRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalRepr MinimalRepr Effect
family =
    RawFamily.make
        (Id.familyR "sumR")
        MinimalRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
