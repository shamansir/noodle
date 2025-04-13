module Example.Toolkit.Minimal.Node.Raw.Sum where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))

import Noodle.Raw.Id (inletR, outletR, familyR) as Id
import Noodle.Raw.Node (Node, InitialInletsValues, InitialOutletsValues) as Raw
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (make, tagAs) as RawShape
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (receive, send) as RawFn
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (make, spawn) as RawFamily
import Noodle.Repr.ValueInChannel (toMaybe, accept) as ViC

import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)
import Example.Toolkit.Minimal.ChRepr (MinimalVRepr(..)) as MinimalRepr
import Example.Toolkit.Minimal.StRepr (MinimalStRepr)
import Example.Toolkit.Minimal.StRepr (MinimalStRepr(..)) as MinimalRepr


shape :: Raw.Shape
shape =
    RawShape.make
        { inlets :
            [ { name : Id.inletR "a", temp : Hot, order : 0, tag : RawShape.tagAs "Int" }
            , { name : Id.inletR "b", temp : Hot, order : 1, tag : RawShape.tagAs "Int" }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "sum", order : 0, tag : RawShape.tagAs "Int" }
            ]
        } -- TODO


defaultInlets :: Raw.InitialInletsValues MinimalVRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.inletR "a") (MinimalRepr.Int 0)
        # Map.insert (Id.inletR "b") (MinimalRepr.Int 0)


defaultOutlets :: Raw.InitialOutletsValues MinimalVRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.outletR "sum") (MinimalRepr.Int 0)


process :: Raw.Process MinimalStRepr MinimalVRepr Effect
process = do
    vicA <- RawFn.receive $ Id.inletR "a"
    vicB <- RawFn.receive $ Id.inletR "b"
    RawFn.send (Id.outletR "sum") $ ViC.accept $ MinimalRepr.Int $ case ViC.toMaybe vicA /\ ViC.toMaybe vicB of
        (Just (MinimalRepr.Int a) /\ Just (MinimalRepr.Int b)) -> a + b
        _ -> 0


makeNode :: Effect (Raw.Node MinimalStRepr MinimalVRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalStRepr MinimalVRepr Effect
family =
    RawFamily.make
        (Id.familyR "sumR")
        MinimalRepr.NoSt
        shape
        defaultInlets
        defaultOutlets
        process
