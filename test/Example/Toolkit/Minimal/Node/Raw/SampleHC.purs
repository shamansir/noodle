module Example.Toolkit.Minimal.Node.Raw.SampleHC where

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

import Example.Toolkit.Minimal.Node.Raw.Sample as Src
import Example.Toolkit.Minimal.Repr (MinimalVRepr, MinimalStRepr)
import Example.Toolkit.Minimal.Repr (MinimalVRepr(..), MinimalStRepr(..)) as MinimalRepr


shape :: Raw.Shape
shape =
    RawShape.make
        { inlets :
            [ { name : Id.inletR "foo", temp : Hot, order : 0 }
            , { name : Id.inletR "c", temp : Hot, order : 1 }
            , { name : Id.inletR "bar", temp : Cold, order : 2 } -- it's cold, unlike in the source
            ] -- FIXME: order should not be necessary here due to the fact we have index
        , outlets :
            [ { name : Id.outletR "foo", order : 0 }
            , { name : Id.outletR "bar", order : 1 }
            ]
        } -- TODO


defaultInlets :: Raw.InletsValues MinimalVRepr
defaultInlets = Src.defaultInlets


defaultOutlets :: Raw.OutletsValues MinimalVRepr
defaultOutlets = Src.defaultOutlets


process :: Raw.Process MinimalStRepr MinimalVRepr Effect
process = Src.process


makeNode :: Effect (Raw.Node MinimalStRepr MinimalVRepr Effect)
makeNode =
    RawFamily.spawn family


family :: Raw.Family MinimalStRepr MinimalVRepr Effect
family =
    RawFamily.make
        (Id.familyR "sampleHCR")
        MinimalRepr.NoSt
        shape
        defaultInlets
        defaultOutlets
        process
