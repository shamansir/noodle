module Example.Toolkit.Minimal.Node.Raw.Stateful where

import Prelude

import Effect (Effect)

import Control.Monad.State.Class (modify_) as State

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

import Example.Toolkit.Minimal.Repr (MinimalRepr)
import Example.Toolkit.Minimal.Repr (MinimalRepr(..)) as MinimalRepr


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


defaultInlets :: Raw.InletsValues MinimalRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.InletR "a") (MinimalRepr.Int 0)
        # Map.insert (Id.InletR "b") (MinimalRepr.Int 0)


defaultOutlets :: Raw.OutletsValues MinimalRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.OutletR "sum") (MinimalRepr.Int 0)


process :: Raw.Process MinimalRepr MinimalRepr Effect
process = do
    mbA <- RawFn.receive $ Id.InletR "a"
    mbB <- RawFn.receive $ Id.InletR "b"
    case mbA /\ mbB of
        (Repr (MinimalRepr.Int a) /\ Repr (MinimalRepr.Int b)) -> do
            State.modify_
                \srepr -> case srepr of
                    MinimalRepr.Str s -> MinimalRepr.Str $ s <> "-" <> show (a + b)
                    _ -> srepr
            RawFn.send (Id.OutletR "out") $ Repr $ MinimalRepr.Int $ a + b
        _ -> pure unit


makeNode :: Effect (Raw.Node MinimalRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalRepr Effect
family =
    RawFamily.make
        (Id.FamilyR { family : "statefulR" })
        MinimalRepr.None
        shape
        defaultInlets
        defaultOutlets
        process
