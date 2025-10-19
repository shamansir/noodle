module Noodle.Unsafe.QuickMake.RawFnShape where

import Prelude

import Data.Array (mapWithIndex) as Array

import Noodle.Id (unsafeInletR, unsafeOutletR)
import Noodle.Raw.Fn.Shape (Shape)
import Noodle.Raw.Fn.Shape (make, tagAs) as RawShape
import Noodle.Fn.Shape.Temperament (Temperament)
import Noodle.Fn.Shape.Temperament as Temp


qmake ::
    { inlets :: Array { name :: String, tag :: String }
    , outlets :: Array { name :: String, tag :: String }
    }
    -> Shape
qmake =
    qmake_ Temp.defaultAlgorithm


qmake_ ::
    Temp.Algorithm ->
    { inlets :: Array { name :: String, tag :: String }
    , outlets :: Array { name :: String, tag :: String }
    }
    -> Shape
qmake_ algo =
    qmake' $ const <<< Temp.byIndex algo


qmake' ::
    (Int -> String -> Temperament) ->
    { inlets :: Array { name :: String, tag :: String }
    , outlets :: Array { name :: String, tag :: String }
    }
    -> Shape
qmake' toTemp { inlets, outlets } =
    RawShape.make
        { inlets  : Array.mapWithIndex
                        (\idx inletRec ->
                            { name : unsafeInletR inletRec.name
                            , order : idx
                            , temp : toTemp idx inletRec.name
                            , tag : RawShape.tagAs inletRec.tag
                            }
                        ) inlets
        , outlets : Array.mapWithIndex
                        (\idx outletRec ->
                            { name : unsafeOutletR outletRec.name
                            , order : idx
                            , tag : RawShape.tagAs outletRec.tag
                            }
                        ) outlets
        }


