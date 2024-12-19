module Example.Toolkit.Minimal.Node.Raw.Concat where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Noodle.Repr.ChRepr (ChRepr(..))
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

import Noodle.Raw.Id (inletR, outletR, familyR) as Id
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Node (Node, InletsValues, OutletsValues) as Raw
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
            [ { name : Id.inletR "left", temp : Hot, order : 0 }
            , { name : Id.inletR "right", temp : Hot, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "out", order : 0 }
            , { name : Id.outletR "len", order : 1 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues MinimalRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.inletR "left") (MinimalRepr.Str "")
        # Map.insert (Id.inletR "right") (MinimalRepr.Str "")


defaultOutlets :: Raw.OutletsValues MinimalRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.outletR "str") (MinimalRepr.Str "")
        # Map.insert (Id.outletR "len") (MinimalRepr.Int 0)


process :: Raw.Process MinimalRepr MinimalRepr Effect
process = do
    mbLeft  <- RawFn.receive $ Id.inletR "left"
    mbRight <- RawFn.receive $ Id.inletR "right"
    case mbLeft /\ mbRight of
        (ChRepr (MinimalRepr.Str left) /\ ChRepr (MinimalRepr.Str right)) ->
            let combined = left <> right
            in do
                RawFn.send (Id.outletR "str") $ ChRepr $ MinimalRepr.Str combined
                RawFn.send (Id.outletR "len") $ ChRepr $ MinimalRepr.Int $ String.length combined
        _ -> pure unit


makeNode :: Effect (Raw.Node MinimalRepr MinimalRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalRepr MinimalRepr Effect
family =
    RawFamily.make
        (Id.familyR "concatR")
        MinimalRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
