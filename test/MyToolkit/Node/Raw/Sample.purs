module Test.MyToolkit.Node.Raw.Sample where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Noodle.Repr (Repr(..))
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

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
            [ { name : "foo", temp : Hot, order : 0 }
            , { name : "c", temp : Hot, order : 1 }
            , { name : "bar", temp : Cold, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : "foo", order : 0 }
            , { name : "bar", order : 1 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues ISRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.InletR "foo") (ISRepr.Int 1)
        # Map.insert (Id.InletR "bar") (ISRepr.Str "5")
        # Map.insert (Id.InletR "c")   (ISRepr.Int 2)


defaultOutlets :: Raw.OutletsValues ISRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.OutletR "foo") (ISRepr.Str "1")
        # Map.insert (Id.OutletR "bar") (ISRepr.Int 12)


process :: Raw.Process ISRepr ISRepr Effect
process = do
    mbFoo  <- RawFn.receive $ Id.InletR "foo"
    mbBar  <- RawFn.receive $ Id.InletR "c"
    mbC    <- RawFn.receive $ Id.InletR "bar"
    case mbFoo /\ mbBar /\ mbC of
        (Repr (ISRepr.Int foo) /\ Repr (ISRepr.Str bar) /\ Repr (ISRepr.Int c)) -> do
            RawFn.send (Id.OutletR "foo") $ Repr $ ISRepr.Str $ show (foo + c) <> bar
            RawFn.send (Id.OutletR "bar") $ Repr $ ISRepr.Int $ foo - c
        _ -> pure unit


makeNode :: Effect (Raw.Node ISRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family ISRepr Effect
family =
    RawFamily.make
        (Id.FamilyR { family : "sampleR" })
        ISRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
