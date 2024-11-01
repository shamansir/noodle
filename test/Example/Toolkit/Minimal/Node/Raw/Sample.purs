module Example.Toolkit.Minimal.Node.Raw.Sample where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Noodle.Repr (Repr(..))
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

import Noodle.Id (unsafeFamilyR, InletR(..), OutletR(..)) as Id
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
            [ { name : "foo", temp : Hot, order : 0 }
            , { name : "c", temp : Hot, order : 1 }
            , { name : "bar", temp : Cold, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : "foo", order : 0 }
            , { name : "bar", order : 1 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues MinimalRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.InletR "foo") (MinimalRepr.Int 1)
        # Map.insert (Id.InletR "bar") (MinimalRepr.Str "5")
        # Map.insert (Id.InletR "c")   (MinimalRepr.Int 2)


defaultOutlets :: Raw.OutletsValues MinimalRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.OutletR "foo") (MinimalRepr.Str "1")
        # Map.insert (Id.OutletR "bar") (MinimalRepr.Int 12)


process :: Raw.Process MinimalRepr MinimalRepr Effect
process = do
    mbFoo  <- RawFn.receive $ Id.InletR "foo"
    mbBar  <- RawFn.receive $ Id.InletR "c"
    mbC    <- RawFn.receive $ Id.InletR "bar"
    case mbFoo /\ mbBar /\ mbC of
        (Repr (MinimalRepr.Int foo) /\ Repr (MinimalRepr.Str bar) /\ Repr (MinimalRepr.Int c)) -> do
            RawFn.send (Id.OutletR "foo") $ Repr $ MinimalRepr.Str $ show (foo + c) <> bar
            RawFn.send (Id.OutletR "bar") $ Repr $ MinimalRepr.Int $ foo - c
        _ -> pure unit


makeNode :: Effect (Raw.Node MinimalRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalRepr Effect
family =
    RawFamily.make
        (Id.unsafeFamilyR "sampleR")
        MinimalRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
