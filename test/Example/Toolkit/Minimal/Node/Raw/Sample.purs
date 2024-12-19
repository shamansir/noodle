module Example.Toolkit.Minimal.Node.Raw.Sample where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Noodle.Repr.ChRepr (ChRepr(..))
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

import Noodle.Raw.Id (inletR, outletR, familyR) as Id
import Noodle.Raw.Node (Node, InletsValues, OutletsValues) as Raw
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (make) as RawShape
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (receive, send) as RawFn
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (make, spawn) as RawFamily

import Example.Toolkit.Minimal.Repr (MinimalVRepr, MinimalStRepr)
import Example.Toolkit.Minimal.Repr (MinimalVRepr(..), MinimalStRepr(..)) as MinimalRepr


shape :: Raw.Shape
shape =
    RawShape.make
        { inlets :
            [ { name : Id.inletR "foo", temp : Hot, order : 0 }
            , { name : Id.inletR "c", temp : Hot, order : 1 }
            , { name : Id.inletR "bar", temp : Cold, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "foo", order : 0 }
            , { name : Id.outletR "bar", order : 1 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues MinimalVRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.inletR "foo") (MinimalRepr.Int 1)
        # Map.insert (Id.inletR "bar") (MinimalRepr.Str "5")
        # Map.insert (Id.inletR "c")   (MinimalRepr.Int 2)


defaultOutlets :: Raw.OutletsValues MinimalVRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.outletR "foo") (MinimalRepr.Str "1")
        # Map.insert (Id.outletR "bar") (MinimalRepr.Int 12)


process :: Raw.Process MinimalStRepr MinimalVRepr Effect
process = do
    mbFoo  <- RawFn.receive $ Id.inletR "foo"
    mbBar  <- RawFn.receive $ Id.inletR "c"
    mbC    <- RawFn.receive $ Id.inletR "bar"
    case mbFoo /\ mbBar /\ mbC of
        (ChRepr (MinimalRepr.Int foo) /\ ChRepr (MinimalRepr.Str bar) /\ ChRepr (MinimalRepr.Int c)) -> do
            RawFn.send (Id.outletR "foo") $ ChRepr $ MinimalRepr.Str $ show (foo + c) <> bar
            RawFn.send (Id.outletR "bar") $ ChRepr $ MinimalRepr.Int $ foo - c
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
