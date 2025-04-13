module Example.Toolkit.Minimal.Node.Raw.Sample where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))

import Noodle.Raw.Id (inletR, outletR, familyR) as Id
import Noodle.Raw.Node (Node, InitialInletsValues, InitialOutletsValues) as Raw
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (make) as RawShape
import Noodle.Raw.Fn.Shape (tagAs)
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
            [ { name : Id.inletR "foo", temp : Hot, order : 0, tag : tagAs "Int" }
            , { name : Id.inletR "c",   temp : Hot, order : 1, tag : tagAs "Str" }
            , { name : Id.inletR "bar", temp : Hot, order : 2, tag : tagAs "Int" }
            ] -- FIXME: order should not be necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "foo", order : 0, tag : tagAs "Str" }
            , { name : Id.outletR "bar", order : 1, tag : tagAs "Int" }
            ]
        } -- TODO


defaultInlets :: Raw.InitialInletsValues MinimalVRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.inletR "foo") (MinimalRepr.Int 1)
        # Map.insert (Id.inletR "bar") (MinimalRepr.Str "5")
        # Map.insert (Id.inletR "c")   (MinimalRepr.Int 2)


defaultOutlets :: Raw.InitialOutletsValues MinimalVRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.outletR "foo") (MinimalRepr.Str "1")
        # Map.insert (Id.outletR "bar") (MinimalRepr.Int 12)


process :: Raw.Process MinimalStRepr MinimalVRepr Effect
process = do
    vicFoo <- RawFn.receive $ Id.inletR "foo"
    vicBar <- RawFn.receive $ Id.inletR "bar"
    vicC   <- RawFn.receive $ Id.inletR "c"
    case ViC.toMaybe vicFoo /\ ViC.toMaybe vicBar /\ ViC.toMaybe vicC of
        (Just (MinimalRepr.Int foo) /\ Just (MinimalRepr.Str bar) /\ Just (MinimalRepr.Int c)) -> do
            RawFn.send (Id.outletR "foo") $ ViC.accept $ MinimalRepr.Str $ show (foo + c) <> bar
            RawFn.send (Id.outletR "bar") $ ViC.accept $ MinimalRepr.Int $ foo - c
        _ -> pure unit


makeNode :: Effect (Raw.Node MinimalStRepr MinimalVRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalStRepr MinimalVRepr Effect
family =
    RawFamily.make
        (Id.familyR "sampleR")
        MinimalRepr.NoSt
        shape
        defaultInlets
        defaultOutlets
        process
