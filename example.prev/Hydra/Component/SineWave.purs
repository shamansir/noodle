module Hydra.Component.SineWave where

import Prelude
import Math (pi, sqrt2)

import Data.Array (snoc, (:))
import Data.Array as Array
import Data.Vec (Vec, (+>), (!!))
import Data.Vec as Vec
import Data.Vec2 (Pos, (<+>))
import Data.Vec2 as V2
import Data.Int (toNumber)
import Data.Foldable (foldr)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))

import Data.Typelevel.Num.Reps (D3, d0, d1, d2)

import Halogen.Svg.Attributes as HSA


type CurvePoints = Vec D3 Pos



build :: Pos -> Number -> Number -> Int -> Array CurvePoints
build pos width amp numHalves =
    let
        xd = pi / 12.0
        y1 = (2.0 * sqrt2) / 7.0 - 1.0 / 7.0
        y2 = (4.0 * sqrt2) / 7.0 - 2.0 / 7.0
        y3 = sqrt2 / 2.0
        y4 = (3.0 * sqrt2) / 7.0 + 2.0 / 7.0
        waveWidth = width / toNumber numHalves
        x = V2.x pos
        y = V2.y pos
        createHalfWaveSegment _ (curX /\ curAmp /\ segments) =
               (curX + waveWidth)
            /\ (curAmp * -1.0)
            /\ (segments `snoc`
                    [    (curX +        xd <+> y + curAmp * y1)
                      +> (curX +  2.0 * xd <+> y + curAmp * y2)
                      +> (curX +  3.0 * xd <+> y + curAmp * y3)
                      +> Vec.empty
                    ,    (curX +  4.0 * xd <+> y + curAmp * y4)
                      +> (curX +  5.0 * xd <+> y + curAmp)
                      +> (curX +  6.0 * xd <+> y + curAmp)
                      +> Vec.empty
                    ,    (curX +  7.0 * xd <+> y + curAmp)
                      +> (curX +  8.0 * xd <+> y + curAmp * y4)
                      +> (curX +  9.0 * xd <+> y + curAmp * y3)
                      +> Vec.empty
                    ,    (curX + 10.0 * xd <+> y + curAmp * y2)
                      +> (curX + 11.0 * xd <+> y + curAmp * y1)
                      +> (curX + 12.0 * xd <+> y)
                      +> Vec.empty
                    ]
               )
    in
        Array.range 1 numHalves
        # foldr createHalfWaveSegment (x /\ amp /\ [])
        # Tuple.snd # Tuple.snd
        # Array.concat


render :: Pos -> Number -> Number -> Int -> Array HSA.PathCommand
render pos width amp numHalves =
      HSA.m HSA.Abs (V2.x pos) (V2.y pos)
    : (curvePointsToPath <$> build pos width amp numHalves)
    where
        curvePointsToPath :: CurvePoints -> HSA.PathCommand
        curvePointsToPath v3 =
            HSA.c HSA.Abs
                (V2.x $ v3 !! d0)
                (V2.y $ v3 !! d0)
                (V2.x $ v3 !! d1)
                (V2.y $ v3 !! d1)
                (V2.x $ v3 !! d2)
                (V2.y $ v3 !! d2)
