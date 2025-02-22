module Example.Toolkit.Minimal.Node.Raw.Concat where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Map (empty, insert) as Map
import Data.Tuple.Nested ((/\))
import Data.String (length) as String

import Noodle.Raw.Id (inletR, outletR, familyR) as Id
import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Raw.Node (Node, InitialInletsValues, InitialOutletsValues) as Raw
import Noodle.Raw.Fn.Shape (Shape) as Raw
import Noodle.Raw.Fn.Shape (make) as RawShape
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Raw.Fn.Process (receive, send) as RawFn
import Noodle.Raw.Fn.Shape (tagAs)
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Raw.Toolkit.Family (make, spawn) as RawFamily
import Noodle.Repr.ValueInChannel (toMaybe, accept) as ViC

import Example.Toolkit.Minimal.Repr (MinimalStRepr, MinimalVRepr)
import Example.Toolkit.Minimal.Repr (MinimalVRepr(..), MinimalStRepr(..)) as MinimalRepr


shape :: Raw.Shape
shape =
    RawShape.make
        { inlets :
            [ { name : Id.inletR "left",  temp : Hot, order : 0, tag : tagAs "Str" }
            , { name : Id.inletR "right", temp : Hot, order : 1, tag : tagAs "Str" }
            ] -- FIXME: order is not necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "out", order : 0, tag : tagAs "Str" }
            , { name : Id.outletR "len", order : 1, tag : tagAs "Int" }
            ]
        } -- TODO


defaultInlets :: Raw.InitialInletsValues MinimalVRepr
defaultInlets =
    Map.empty
        # Map.insert (Id.inletR "left")  (MinimalRepr.Str "")
        # Map.insert (Id.inletR "right") (MinimalRepr.Str "")


defaultOutlets :: Raw.InitialOutletsValues MinimalVRepr
defaultOutlets =
    Map.empty
        # Map.insert (Id.outletR "str") (MinimalRepr.Str "")
        # Map.insert (Id.outletR "len") (MinimalRepr.Int 0)


process :: Raw.Process MinimalStRepr MinimalVRepr Effect
process = do
    mbLeft  <- ViC.toMaybe <$> (RawFn.receive $ Id.inletR "left")
    mbRight <- ViC.toMaybe <$> (RawFn.receive $ Id.inletR "right")
    case mbLeft /\ mbRight of
        (Just (MinimalRepr.Str left) /\ Just (MinimalRepr.Str right)) ->
            let combined = left <> right
            in do
                RawFn.send (Id.outletR "str") $ ViC.accept $ MinimalRepr.Str combined
                RawFn.send (Id.outletR "len") $ ViC.accept $ MinimalRepr.Int $ String.length combined
        _ -> pure unit


makeNode :: Effect (Raw.Node MinimalStRepr MinimalVRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalStRepr MinimalVRepr Effect
family =
    RawFamily.make
        (Id.familyR "concatR")
        MinimalRepr.NoSt
        shape
        defaultInlets
        defaultOutlets
        process
