module Test.MyToolkit.Node.Raw.Stateful where

import Prelude

import Effect (Effect)

import Control.Monad.State.Class (modify_) as State

import Data.Map (empty, insert) as Map
import Data.Repr (Repr(..))
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

import Noodle.Id (FamilyR(..), InletR(..), OutletR(..)) as Id
import Noodle.Raw.Node (Node, InletsValues, OutletsValues) as Raw
import Noodle.Raw.Fn.Shape.Temperament (Temperament(..))
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
    case mbA /\ mbB of
        (Repr (ISRepr.Int a) /\ Repr (ISRepr.Int b)) -> do
            State.modify_
                \srepr -> case srepr of
                    ISRepr.Str s -> ISRepr.Str $ s <> "-" <> show (a + b)
                    _ -> srepr
            RawFn.send (Id.OutletR "out") $ Repr $ ISRepr.Int $ a + b
        _ -> pure unit


node :: Effect (Raw.Node ISRepr Effect)
node =
    RawFamily.spawn family


family :: Raw.Family ISRepr Effect
family =
    RawFamily.make
        (Id.FamilyR { family : "statefulR" })
        ISRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
