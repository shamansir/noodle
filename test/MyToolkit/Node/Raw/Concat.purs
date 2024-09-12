module Test.MyToolkit.Node.Raw.Concat where

import Prelude

import Effect (Effect)

import Data.Map (empty, insert) as Map
import Data.Repr (Repr(..))
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

import Test.MyToolkit.Repr (ISRepr)
import Test.MyToolkit.Repr (ISRepr(..)) as ISRepr


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


defaultInlets :: Raw.InletsValues ISRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.InletR "left") (ISRepr.Str "")
        # Map.insert (Id.InletR "right") (ISRepr.Str "")


defaultOutlets :: Raw.OutletsValues ISRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.OutletR "str") (ISRepr.Str "")
        # Map.insert (Id.OutletR "len") (ISRepr.Int 0)


process :: Raw.Process ISRepr ISRepr Effect
process = do
    mbLeft  <- RawFn.receive $ Id.InletR "left"
    mbRight <- RawFn.receive $ Id.InletR "right"
    case mbLeft /\ mbRight of
        (Repr (ISRepr.Str left) /\ Repr (ISRepr.Str right)) ->
            let combined = left <> right
            in do
                RawFn.send (Id.OutletR "str") $ Repr $ ISRepr.Str combined
                RawFn.send (Id.OutletR "len") $ Repr $ ISRepr.Int $ String.length combined
        _ -> pure unit


node :: Effect (Raw.Node ISRepr Effect)
node =
    RawFamily.spawn family


family :: Raw.Family ISRepr Effect
family =
    RawFamily.make
        (Id.FamilyR { family : "concatR" })
        ISRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
