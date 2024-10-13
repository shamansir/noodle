module Example.Toolkit.Minimal.Node.Raw.Concat where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Noodle.Repr (Repr(..))
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

import Noodle.Id (FamilyR(..), InletR(..), OutletR(..)) as Id
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
            [ { name : "left", temp : Hot, order : 0 }
            , { name : "right", temp : Hot, order : 1 }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : "out", order : 0 }
            , { name : "len", order : 1 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues MinimalRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.InletR "left") (MinimalRepr.Str "")
        # Map.insert (Id.InletR "right") (MinimalRepr.Str "")


defaultOutlets :: Raw.OutletsValues MinimalRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.OutletR "str") (MinimalRepr.Str "")
        # Map.insert (Id.OutletR "len") (MinimalRepr.Int 0)


process :: Raw.Process MinimalRepr MinimalRepr Effect
process = do
    mbLeft  <- RawFn.receive $ Id.InletR "left"
    mbRight <- RawFn.receive $ Id.InletR "right"
    case mbLeft /\ mbRight of
        (Repr (MinimalRepr.Str left) /\ Repr (MinimalRepr.Str right)) ->
            let combined = left <> right
            in do
                RawFn.send (Id.OutletR "str") $ Repr $ MinimalRepr.Str combined
                RawFn.send (Id.OutletR "len") $ Repr $ MinimalRepr.Int $ String.length combined
        _ -> pure unit


makeNode :: Effect (Raw.Node MinimalRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalRepr Effect
family =
    RawFamily.make
        (Id.FamilyR { family : "concatR" })
        MinimalRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
