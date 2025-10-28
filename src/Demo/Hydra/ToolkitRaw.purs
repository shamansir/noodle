module HydraTk.ToolkitRaw where

import Prelude

import Color as Color
import Effect.Class (class MonadEffect)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil)
import Noodle.Id (FamilyR, NodeR, toolkitR, family, group, unsafeGroupR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasChRepr, class InitPatchState, class FromToPatchState, markGroup)
import Noodle.Toolkit (empty) as Toolkit
import Noodle.Repr.HasFallback (fallback)
import Noodle.Unsafe.QuickMake.RawToolkit (qregister) as Toolkit
import Noodle.Unsafe.RawProcess as RP
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Fn.Signature (sig, class PossiblyToSignature)
import Noodle.Fn.Signature (in_, inx_, out_, outx_, toChanneled) as Sig
import Cli.Class.CliRenderer (class CliEditor)
import Web.Class.WebRenderer (class WebEditor)
import Web.Class.WebRenderer (InletPath) as WR
import Web.Components.ValueEditor (ValueEditor)
import Web.Components.ValueEditor (EditorId) as ValueEditor
import Web.Components.ValueEditor.Numeric as NumericVE
import HydraTk.Repr.State (StateRepr)
import HydraTk.Repr.Wrap (WrapRepr)
import HydraTk.Patch (PState)
import HydraTk.Patch (init) as Patch
import HydraTk.Types as HYDRA
import HydraTk.Repr.Wrap as HYDRAW
import Data.Tuple.Nested ((/\))

foreign import data HYDRA :: ToolkitKey

toolkit :: forall m. MonadEffect m => Toolkit HYDRA TNil StateRepr WrapRepr m
toolkit = Toolkit.empty (Proxy :: _ HYDRA) (Id.toolkitR "Hydra")
  #
    ( {- number -}
      Toolkit.qregister "number" [] [ { name: "out", tag: "Value", value: Just "N 0.0" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- pi -}
      Toolkit.qregister "pi" [] [ { name: "out", tag: "Value", value: Just "PI V" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- array -}
      Toolkit.qregister "array" [] [ { name: "out", tag: "Value", value: Just "VA %%%%" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- expression -}
      Toolkit.qregister "expression" [] [ { name: "out", tag: "Value", value: Just "D /----/" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- time -}
      Toolkit.qregister "time" [] [ { name: "time", tag: "Value", value: Just "T V" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- mouse -}
      Toolkit.qregister "mouse" []
        [ { name: "x", tag: "Value", value: Just "MX V" }
        , { name: "y", tag: "Value", value: Just "MY V" }
        ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- glslFn -}
      Toolkit.qregister "glslFn" [] [ { name: "out", tag: "Value", value: Just "D /----/" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- noise -}
      Toolkit.qregister "noise"
        [ { name: "scale", tag: "Value", value: Just "N 10.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_scale <- RP.receiveViC "scale"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                scale <- vic_scale
                offset <- vic_offset
                pure $ HYDRA.Start $ HYDRA.From $ HYDRA.Noise { scale, offset }
    )
  #
    ( {- voronoi -}
      Toolkit.qregister "voronoi"
        [ { name: "scale", tag: "Value", value: Just "N 5.0" }
        , { name: "speed", tag: "Value", value: Just "N 0.3" }
        , { name: "blending", tag: "Value", value: Just "N 0.3" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_scale <- RP.receiveViC "scale"
            vic_speed <- RP.receiveViC "speed"
            vic_blending <- RP.receiveViC "blending"
            RP.sendViC "out" $ do
                scale <- vic_scale
                speed <- vic_speed
                blending <- vic_blending
                pure $ HYDRA.Start $ HYDRA.From $ HYDRA.Voronoi { scale, speed, blending }
    )
  #
    ( {- osc -}
      Toolkit.qregister "osc"
        [ { name: "frequency", tag: "Value", value: Just "N 60.0" }
        , { name: "sync", tag: "Value", value: Just "N 0.1" }
        , { name: "offset", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_frequency <- RP.receiveViC "frequency"
            vic_sync <- RP.receiveViC "sync"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                frequency <- vic_frequency
                sync <- vic_sync
                offset <- vic_offset
                pure $ HYDRA.Start $ HYDRA.From $ HYDRA.Osc { frequency, sync, offset }
    )
  #
    ( {- shape -}
      Toolkit.qregister "shape"
        [ { name: "sides", tag: "Value", value: Just "N 60.0" }
        , { name: "radius", tag: "Value", value: Just "N 0.3" }
        , { name: "smoothing", tag: "Value", value: Just "N 0.01" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_sides <- RP.receiveViC "sides"
            vic_radius <- RP.receiveViC "radius"
            vic_smoothing <- RP.receiveViC "smoothing"
            RP.sendViC "out" $ do
                sides <- vic_sides
                radius <- vic_radius
                smoothing <- vic_smoothing
                pure $ HYDRA.Start $ HYDRA.From $ HYDRA.Shape { sides, radius, smoothing }
    )
  #
    ( {- gradient -}
      Toolkit.qregister "gradient" [ { name: "speed", tag: "Value", value: Nothing } ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "out" $ do
                speed <- vic_speed
                pure $ HYDRA.Start $ HYDRA.From $ HYDRA.Gradient { speed }
    )
  #
    ( {- src -}
      Toolkit.qregister "src" [ { name: "load", tag: "OutputN", value: Nothing } ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_load <- RP.receiveViC "load"
            RP.sendViC "out" $ do
                load <- vic_load
                pure $ HYDRA.Start $ HYDRA.Load load
    )
  #
    ( {- solid -}
      Toolkit.qregister "solid"
        [ { name: "r", tag: "Value", value: Nothing }
        , { name: "g", tag: "Value", value: Nothing }
        , { name: "b", tag: "Value", value: Nothing }
        , { name: "a", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_r <- RP.receiveViC "r"
            vic_g <- RP.receiveViC "g"
            vic_b <- RP.receiveViC "b"
            vic_a <- RP.receiveViC "a"
            RP.sendViC "out" $ do
                r <- vic_r
                g <- vic_g
                b <- vic_b
                a <- vic_a
                pure $ HYDRA.Start $ HYDRA.From $ HYDRA.Solid { r, g, b, a }
    )
  #
    ( {- prev -}
      Toolkit.qregister "prev" [ { name: "todo", tag: "TODO", value: Just "TODO" } ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            RP.sendViC "out" $ do

                pure $ HYDRA.Empty
    )
  #
    ( {- rotate -}
      Toolkit.qregister "rotate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "angle", tag: "Value", value: Just "N 10.0" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_angle <- RP.receiveViC "angle"
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "out" $ do
                what <- vic_what
                angle <- vic_angle
                speed <- vic_speed
                pure $ HYDRA.Geometry what $ HYDRA.GRotate { angle, speed }
    )
  #
    ( {- scale -}
      Toolkit.qregister "scale"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.5" }
        , { name: "xMult", tag: "Value", value: Just "N 1.0" }
        , { name: "yMult", tag: "Value", value: Just "N 1.0" }
        , { name: "offsetX", tag: "Value", value: Just "N 0.5" }
        , { name: "offsetY", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_amount <- RP.receiveViC "amount"
            vic_xMult <- RP.receiveViC "xMult"
            vic_yMult <- RP.receiveViC "yMult"
            vic_offsetX <- RP.receiveViC "offsetX"
            vic_offsetY <- RP.receiveViC "offsetY"
            RP.sendViC "out" $ do
                what <- vic_what
                amount <- vic_amount
                xMult <- vic_xMult
                yMult <- vic_yMult
                offsetX <- vic_offsetX
                offsetY <- vic_offsetY
                pure $ HYDRA.Geometry what $ HYDRA.GScale { amount, xMult, yMult, offsetX, offsetY }
    )
  #
    ( {- pixelate -}
      Toolkit.qregister "pixelate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "pixelX", tag: "Value", value: Just "N 20.0" }
        , { name: "pixelY", tag: "Value", value: Just "N 20.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_pixelX <- RP.receiveViC "pixelX"
            vic_pixelY <- RP.receiveViC "pixelY"
            RP.sendViC "out" $ do
                what <- vic_what
                pixelX <- vic_pixelX
                pixelY <- vic_pixelY
                pure $ HYDRA.Geometry what $ HYDRA.GPixelate { pixelX, pixelY }
    )
  #
    ( {- repeat -}
      Toolkit.qregister "repeat"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "repeatX", tag: "Value", value: Just "N 3.0" }
        , { name: "repeatY", tag: "Value", value: Just "N 3.0" }
        , { name: "offsetX", tag: "Value", value: Just "N 0.0" }
        , { name: "offsetY", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_repeatX <- RP.receiveViC "repeatX"
            vic_repeatY <- RP.receiveViC "repeatY"
            vic_offsetX <- RP.receiveViC "offsetX"
            vic_offsetY <- RP.receiveViC "offsetY"
            RP.sendViC "out" $ do
                what <- vic_what
                repeatX <- vic_repeatX
                repeatY <- vic_repeatY
                offsetX <- vic_offsetX
                offsetY <- vic_offsetY
                pure $ HYDRA.Geometry what $ HYDRA.GRepeat { repeatX, repeatY, offsetX, offsetY }
    )
  #
    ( {- repeatX -}
      Toolkit.qregister "repeatX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_reps <- RP.receiveViC "reps"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                reps <- vic_reps
                offset <- vic_offset
                pure $ HYDRA.Geometry what $ HYDRA.GRepeatX { reps, offset }
    )
  #
    ( {- repeatY -}
      Toolkit.qregister "repeatY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_reps <- RP.receiveViC "reps"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                reps <- vic_reps
                offset <- vic_offset
                pure $ HYDRA.Geometry what $ HYDRA.GRepeatY { reps, offset }
    )
  #
    ( {- kaleid -}
      Toolkit.qregister "kaleid"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "nSides", tag: "Value", value: Just "N 3.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_nSides <- RP.receiveViC "nSides"
            RP.sendViC "out" $ do
                what <- vic_what
                nSides <- vic_nSides
                pure $ HYDRA.Geometry what $ HYDRA.GKaleid { nSides }
    )
  #
    ( {- scroll -}
      Toolkit.qregister "scroll"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollX", tag: "Value", value: Just "N 0.5" }
        , { name: "scrollY", tag: "Value", value: Just "N 0.5" }
        , { name: "speedX", tag: "Value", value: Nothing }
        , { name: "speedY", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scrollX <- RP.receiveViC "scrollX"
            vic_scrollY <- RP.receiveViC "scrollY"
            vic_speedX <- RP.receiveViC "speedX"
            vic_speedY <- RP.receiveViC "speedY"
            RP.sendViC "out" $ do
                what <- vic_what
                scrollX <- vic_scrollX
                scrollY <- vic_scrollY
                speedX <- vic_speedX
                speedY <- vic_speedY
                pure $ HYDRA.Geometry what $ HYDRA.GScroll { scrollX, scrollY, speedX, speedY }
    )
  #
    ( {- scrollX -}
      Toolkit.qregister "scrollX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollX", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scrollX <- RP.receiveViC "scrollX"
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "out" $ do
                what <- vic_what
                scrollX <- vic_scrollX
                speed <- vic_speed
                pure $ HYDRA.Geometry what $ HYDRA.GScrollX { scrollX, speed }
    )
  #
    ( {- scrollY -}
      Toolkit.qregister "scrollY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollY", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scrollY <- RP.receiveViC "scrollY"
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "out" $ do
                what <- vic_what
                scrollY <- vic_scrollY
                speed <- vic_speed
                pure $ HYDRA.Geometry what $ HYDRA.GScrollY { scrollY, speed }
    )
  #
    ( {- posterize -}
      Toolkit.qregister "posterize"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "bins", tag: "Value", value: Just "N 3.0" }
        , { name: "gamma", tag: "Value", value: Just "N 0.6" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_bins <- RP.receiveViC "bins"
            vic_gamma <- RP.receiveViC "gamma"
            RP.sendViC "out" $ do
                what <- vic_what
                bins <- vic_bins
                gamma <- vic_gamma
                pure $ HYDRA.Filter what $ HYDRA.Posterize { bins, gamma }
    )
  #
    ( {- shift -}
      Toolkit.qregister "shift"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "r", tag: "Value", value: Just "N 0.5" }
        , { name: "g", tag: "Value", value: Just "N 0.5" }
        , { name: "b", tag: "Value", value: Just "N 0.5" }
        , { name: "a", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_r <- RP.receiveViC "r"
            vic_g <- RP.receiveViC "g"
            vic_b <- RP.receiveViC "b"
            vic_a <- RP.receiveViC "a"
            RP.sendViC "out" $ do
                what <- vic_what
                r <- vic_r
                g <- vic_g
                b <- vic_b
                a <- vic_a
                pure $ HYDRA.Filter what $ HYDRA.Shift { r, g, b, a }
    )
  #
    ( {- invert -}
      Toolkit.qregister "invert"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                amount <- vic_amount
                pure $ HYDRA.Filter what $ HYDRA.Invert amount
    )
  #
    ( {- contrast -}
      Toolkit.qregister "contrast"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.6" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                amount <- vic_amount
                pure $ HYDRA.Filter what $ HYDRA.Contrast amount
    )
  #
    ( {- brightness -}
      Toolkit.qregister "brightness"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.4" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                amount <- vic_amount
                pure $ HYDRA.Filter what $ HYDRA.Brightness amount
    )
  #
    ( {- luma -}
      Toolkit.qregister "luma"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "threshold", tag: "Value", value: Just "N 0.5" }
        , { name: "tolerance", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_threshold <- RP.receiveViC "threshold"
            vic_tolerance <- RP.receiveViC "tolerance"
            RP.sendViC "out" $ do
                what <- vic_what
                threshold <- vic_threshold
                tolerance <- vic_tolerance
                pure $ HYDRA.Filter what $ HYDRA.Luma { threshold, tolerance }
    )
  #
    ( {- thresh -}
      Toolkit.qregister "thresh"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "threshold", tag: "Value", value: Just "N 0.5" }
        , { name: "tolerance", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_threshold <- RP.receiveViC "threshold"
            vic_tolerance <- RP.receiveViC "tolerance"
            RP.sendViC "out" $ do
                what <- vic_what
                threshold <- vic_threshold
                tolerance <- vic_tolerance
                pure $ HYDRA.Filter what $ HYDRA.Thresh { threshold, tolerance }
    )
  #
    ( {- color -}
      Toolkit.qregister "color"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "r", tag: "Value", value: Just "N 1.0" }
        , { name: "g", tag: "Value", value: Just "N 1.0" }
        , { name: "b", tag: "Value", value: Just "N 1.0" }
        , { name: "a", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_r <- RP.receiveViC "r"
            vic_g <- RP.receiveViC "g"
            vic_b <- RP.receiveViC "b"
            vic_a <- RP.receiveViC "a"
            RP.sendViC "out" $ do
                what <- vic_what
                r <- vic_r
                g <- vic_g
                b <- vic_b
                a <- vic_a
                pure $ HYDRA.Filter what $ HYDRA.Color { r, g, b, a }
    )
  #
    ( {- saturate -}
      Toolkit.qregister "saturate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 2.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                amount <- vic_amount
                pure $ HYDRA.Filter what $ HYDRA.Saturate amount
    )
  #
    ( {- hue -}
      Toolkit.qregister "hue"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "hue", tag: "Value", value: Just "N 0.4" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_hue <- RP.receiveViC "hue"
            RP.sendViC "out" $ do
                what <- vic_what
                hue <- vic_hue
                pure $ HYDRA.Filter what $ HYDRA.Hue hue
    )
  #
    ( {- colorama -}
      Toolkit.qregister "colorama"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.005" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                amount <- vic_amount
                pure $ HYDRA.Filter what $ HYDRA.Colorama amount
    )
  #
    ( {- sum -}
      Toolkit.qregister "sum"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- r -}
      Toolkit.qregister "r"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scale <- RP.receiveViC "scale"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                scale <- vic_scale
                offset <- vic_offset
                pure $ HYDRA.Filter what $ HYDRA.R { scale, offset }
    )
  #
    ( {- b -}
      Toolkit.qregister "b"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scale <- RP.receiveViC "scale"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                scale <- vic_scale
                offset <- vic_offset
                pure $ HYDRA.Filter what $ HYDRA.B { scale, offset }
    )
  #
    ( {- g -}
      Toolkit.qregister "g"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scale <- RP.receiveViC "scale"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                scale <- vic_scale
                offset <- vic_offset
                pure $ HYDRA.Filter what $ HYDRA.G { scale, offset }
    )
  #
    ( {- a -}
      Toolkit.qregister "a"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "scale", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_scale <- RP.receiveViC "scale"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                scale <- vic_scale
                offset <- vic_offset
                pure $ HYDRA.Filter what $ HYDRA.A { scale, offset }
    )
  #
    ( {- add -}
      Toolkit.qregister "add"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Add amount
    )
  #
    ( {- sub -}
      Toolkit.qregister "sub"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Sub amount
    )
  #
    ( {- layer -}
      Toolkit.qregister "layer"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Layer amount
    )
  #
    ( {- blend -}
      Toolkit.qregister "blend"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Blend amount
    )
  #
    ( {- mult -}
      Toolkit.qregister "mult"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Mult amount
    )
  #
    ( {- diff -}
      Toolkit.qregister "diff"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Diff
    )
  #
    ( {- mask -}
      Toolkit.qregister "mask"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                pure $ HYDRA.BlendOf { what, with } $ HYDRA.Mask
    )
  #
    ( {- modulateRepeat -}
      Toolkit.qregister "modulateRepeat"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "repeatX", tag: "Value", value: Just "N 3.0" }
        , { name: "repeatY", tag: "Value", value: Just "N 3.0" }
        , { name: "offsetX", tag: "Value", value: Just "N 0.5" }
        , { name: "offsetY", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_repeatX <- RP.receiveViC "repeatX"
            vic_repeatY <- RP.receiveViC "repeatY"
            vic_offsetX <- RP.receiveViC "offsetX"
            vic_offsetY <- RP.receiveViC "offsetY"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                repeatX <- vic_repeatX
                repeatY <- vic_repeatY
                offsetX <- vic_offsetX
                offsetY <- vic_offsetY
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModRepeat { repeatX, repeatY, offsetX, offsetY }
    )
  #
    ( {- modulateRepeatX -}
      Toolkit.qregister "modulateRepeatX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_reps <- RP.receiveViC "reps"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                reps <- vic_reps
                offset <- vic_offset
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModRepeatX { reps, offset }
    )
  #
    ( {- modulateRepeatY -}
      Toolkit.qregister "modulateRepeatY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "reps", tag: "Value", value: Just "N 3.0" }
        , { name: "offset", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_reps <- RP.receiveViC "reps"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                reps <- vic_reps
                offset <- vic_offset
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModRepeatY { reps, offset }
    )
  #
    ( {- modulateKaleid -}
      Toolkit.qregister "modulateKaleid"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "nSides", tag: "Value", value: Just "N 3.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_nSides <- RP.receiveViC "nSides"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                nSides <- vic_nSides
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModKaleid { nSides }
    )
  #
    ( {- modulateScrollX -}
      Toolkit.qregister "modulateScrollX"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollX", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_scrollX <- RP.receiveViC "scrollX"
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                scrollX <- vic_scrollX
                speed <- vic_speed
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModScrollX { scrollX, speed }
    )
  #
    ( {- modulateScrollY -}
      Toolkit.qregister "modulateScrollY"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "scrollY", tag: "Value", value: Just "N 0.5" }
        , { name: "speed", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_scrollY <- RP.receiveViC "scrollY"
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                scrollY <- vic_scrollY
                speed <- vic_speed
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModScrollY { scrollY, speed }
    )
  #
    ( {- modulate -}
      Toolkit.qregister "modulate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 0.1" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.Modulate amount
    )
  #
    ( {- modulateScale -}
      Toolkit.qregister "modulateScale"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "multiple", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_multiple <- RP.receiveViC "multiple"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                multiple <- vic_multiple
                offset <- vic_offset
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModScale { multiple, offset }
    )
  #
    ( {- modulatePixelate -}
      Toolkit.qregister "modulatePixelate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "multiple", tag: "Value", value: Just "N 10.0" }
        , { name: "offset", tag: "Value", value: Just "N 3.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_multiple <- RP.receiveViC "multiple"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                multiple <- vic_multiple
                offset <- vic_offset
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModPixelate { multiple, offset }
    )
  #
    ( {- modulateRotate -}
      Toolkit.qregister "modulateRotate"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "multiple", tag: "Value", value: Just "N 1.0" }
        , { name: "offset", tag: "Value", value: Nothing }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_multiple <- RP.receiveViC "multiple"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                multiple <- vic_multiple
                offset <- vic_offset
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModRotate { multiple, offset }
    )
  #
    ( {- modulateHue -}
      Toolkit.qregister "modulateHue"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "with", tag: "Texture", value: Just "EMP T" }
        , { name: "amount", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "out", tag: "Texture", value: Just "EMP T" } ] $ do
            vic_what <- RP.receiveViC "what"
            vic_with <- RP.receiveViC "with"
            vic_amount <- RP.receiveViC "amount"
            RP.sendViC "out" $ do
                what <- vic_what
                with <- vic_with
                amount <- vic_amount
                pure $ HYDRA.ModulateWith { what, with } $ HYDRA.ModHue amount
    )
  #
    ( {- initCam -}
      Toolkit.qregister "initCam"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "index", tag: "Value", value: Just "0 V" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- initImage -}
      Toolkit.qregister "initImage"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "url", tag: "Url", value: Nothing }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- initVideo -}
      Toolkit.qregister "initVideo"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "url", tag: "Url", value: Nothing }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- init -}
      Toolkit.qregister "init" [ { name: "options", tag: "SourceOptions", value: Nothing } ] [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- initStream -}
      Toolkit.qregister "initStream"
        [ { name: "src", tag: "SourceN", value: Nothing }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- initScreen -}
      Toolkit.qregister "initScreen" [] [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- render -}
      Toolkit.qregister "render" [ { name: "what", tag: "RenderTarget", value: Just "ALL 4" } ] [] $
        do
          {- render :: PROCESS GOES HERE -}
          pure unit
    )
  #
    ( {- update -}
      Toolkit.qregister "update" [ { name: "fn", tag: "UpdateFn", value: Nothing } ] [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- setResolution -}
      Toolkit.qregister "setResolution"
        [ { name: "width", tag: "Value", value: Just "N 100.0" }
        , { name: "height", tag: "Value", value: Just "N 100.0" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- hush -}
      Toolkit.qregister "hush" [] [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- setFunction -}
      Toolkit.qregister "setFunction" [ { name: "fn", tag: "GlslFn", value: Nothing } ] [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- speed -}
      Toolkit.qregister "speed" [ { name: "v", tag: "Value", value: Just "N 1.0" } ] [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- bpm -}
      Toolkit.qregister "bpm" [ { name: "v", tag: "Value", value: Just "N 30.0" } ] [] $ do
            {- bpm -} pure unit
    )
  #
    ( {- width -}
      Toolkit.qregister "width" [] [ { name: "w", tag: "Value", value: Just "W V" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- height -}
      Toolkit.qregister "height" [] [ { name: "h", tag: "Value", value: Just "H V" } ] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- fast -}
      Toolkit.qregister "fast"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "speed", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ do
            vic_arr <- RP.receiveViC "arr"
            vic_speed <- RP.receiveViC "speed"
            RP.sendViC "arr" $ do
                arr <- vic_arr
                speed <- vic_speed
                pure $ HYDRA.VArray arr $ HYDRA.Fast speed
    )
  #
    ( {- smooth -}
      Toolkit.qregister "smooth"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "smooth", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ do
            vic_arr <- RP.receiveViC "arr"
            vic_smooth <- RP.receiveViC "smooth"
            RP.sendViC "arr" $ do
                arr <- vic_arr
                smooth <- vic_smooth
                pure $ HYDRA.VArray arr $ HYDRA.Smooth smooth
    )
  #
    ( {- ease -}
      Toolkit.qregister "ease"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "ease", tag: "Ease", value: Just "LIN E" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ do
            vic_arr <- RP.receiveViC "arr"
            vic_ease <- RP.receiveViC "ease"
            RP.sendViC "arr" $ do
                arr <- vic_arr
                ease <- vic_ease
                pure $ HYDRA.VArray arr ease
    )
  #
    ( {- offset -}
      Toolkit.qregister "offset"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "offset", tag: "Value", value: Just "N 0.5" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ do
            vic_arr <- RP.receiveViC "arr"
            vic_offset <- RP.receiveViC "offset"
            RP.sendViC "arr" $ do
                arr <- vic_arr
                offset <- vic_offset
                pure $ HYDRA.VArray arr $ HYDRA.Offset offset
    )
  #
    ( {- fit -}
      Toolkit.qregister "fit"
        [ { name: "arr", tag: "Values", value: Just "%%%%" }
        , { name: "low", tag: "Value", value: Just "N 0.0" }
        , { name: "high", tag: "Value", value: Just "N 1.0" }
        ]
        [ { name: "arr", tag: "Value", value: Just "VA %%%%" } ] $ do
            vic_arr <- RP.receiveViC "arr"
            vic_low <- RP.receiveViC "low"
            vic_high <- RP.receiveViC "high"
            RP.sendViC "arr" $ do
                arr <- vic_arr
                low <- vic_low
                high <- vic_high
                pure $ HYDRA.VArray arr $ HYDRA.Fit { low, high }
    )
  #
    ( {- fft -}
      Toolkit.qregister "fft" [ { name: "bin", tag: "AudioBin", value: Just "@0" } ]
        [ { name: "fft", tag: "Value", value: Just "0 V" } ] $ do
            vic_bin <- RP.receiveViC "bin"
            RP.sendViC "fft" $ do
                bin <- vic_bin
                pure $ HYDRA.Fft bin
    )
  #
    ( {- setSmooth -}
      Toolkit.qregister "setSmooth"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "smooth", tag: "Value", value: Just "N 0.4" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- setCutoff -}
      Toolkit.qregister "setCutoff"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "cutoff", tag: "Value", value: Just "N 2.0" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- setBins -}
      Toolkit.qregister "setBins"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "numBins", tag: "Value", value: Just "N 4.0" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- setScale -}
      Toolkit.qregister "setScale"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "scale", tag: "Value", value: Just "N 10.0" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- hide -}
      Toolkit.qregister "hide"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- show -}
      Toolkit.qregister "show"
        [ { name: "audio", tag: "Audio", value: Just "SIL" }
        , { name: "todo", tag: "TODO", value: Just "TODO" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )
  #
    ( {- out -}
      Toolkit.qregister "out"
        [ { name: "what", tag: "Texture", value: Just "EMP T" }
        , { name: "target", tag: "RenderTarget", value: Just "ALL 4" }
        ]
        [] $ {- EMPTY PROCESS -}
            pure unit
    )


instance IsToolkit HYDRA where
  name _ = "Hydra"
  groupOf _ = Id.family
    >>>
      ( case _ of
          "number" -> "feed"
          "pi" -> "feed"
          "array" -> "feed"
          "expression" -> "feed"
          "time" -> "feed"
          "mouse" -> "feed"
          "glslFn" -> "feed"
          "noise" -> "source"
          "voronoi" -> "source"
          "osc" -> "source"
          "shape" -> "source"
          "gradient" -> "source"
          "src" -> "source"
          "solid" -> "source"
          "prev" -> "source"
          "rotate" -> "geometry"
          "scale" -> "geometry"
          "pixelate" -> "geometry"
          "repeat" -> "geometry"
          "repeatX" -> "geometry"
          "repeatY" -> "geometry"
          "kaleid" -> "geometry"
          "scroll" -> "geometry"
          "scrollX" -> "geometry"
          "scrollY" -> "geometry"
          "posterize" -> "color"
          "shift" -> "color"
          "invert" -> "color"
          "contrast" -> "color"
          "brightness" -> "color"
          "luma" -> "color"
          "thresh" -> "color"
          "color" -> "color"
          "saturate" -> "color"
          "hue" -> "color"
          "colorama" -> "color"
          "sum" -> "color"
          "r" -> "color"
          "b" -> "color"
          "g" -> "color"
          "a" -> "color"
          "add" -> "blend"
          "sub" -> "blend"
          "layer" -> "blend"
          "blend" -> "blend"
          "mult" -> "blend"
          "diff" -> "blend"
          "mask" -> "blend"
          "modulateRepeat" -> "modulate"
          "modulateRepeatX" -> "modulate"
          "modulateRepeatY" -> "modulate"
          "modulateKaleid" -> "modulate"
          "modulateScrollX" -> "modulate"
          "modulateScrollY" -> "modulate"
          "modulate" -> "modulate"
          "modulateScale" -> "modulate"
          "modulatePixelate" -> "modulate"
          "modulateRotate" -> "modulate"
          "modulateHue" -> "modulate"
          "initCam" -> "extsource"
          "initImage" -> "extsource"
          "initVideo" -> "extsource"
          "init" -> "extsource"
          "initStream" -> "extsource"
          "initScreen" -> "extsource"
          "render" -> "synth"
          "update" -> "synth"
          "setResolution" -> "synth"
          "hush" -> "synth"
          "setFunction" -> "synth"
          "speed" -> "synth"
          "bpm" -> "synth"
          "width" -> "synth"
          "height" -> "synth"
          "fast" -> "array"
          "smooth" -> "array"
          "ease" -> "array"
          "offset" -> "array"
          "fit" -> "array"
          "fft" -> "audio"
          "setSmooth" -> "audio"
          "setCutoff" -> "audio"
          "setBins" -> "audio"
          "setScale" -> "audio"
          "hide" -> "audio"
          "show" -> "audio"
          "out" -> "out"
          _ -> "unknown"
      )
    >>> Id.unsafeGroupR


instance MarkToolkit HYDRA where
  markGroup _ = Id.group >>>
    ( case _ of
        "feed" -> Color.rgb 6 90 181
        "source" -> Color.rgb 255 163 0
        "geometry" -> Color.rgb 190 18 80
        "color" -> Color.rgb 62 99 123 -- FIXME: fails: Color.rgb 17 29 53
        "blend" -> Color.rgb 255 127 102
        "modulate" -> Color.rgb 255 230 102
        "extsource" -> Color.rgb 179 255 102
        "synth" -> Color.rgb 102 255 127
        "array" -> Color.rgb 102 255 230
        "audio" -> Color.rgb 102 179 255
        "out" -> Color.rgb 128 102 255
        _ -> Color.rgb 255 255 255
    )
  markFamily ptk = const <<< markGroup ptk


instance MonadEffect m => InitPatchState HYDRA PState m where
  initPatch :: Proxy _ -> m PState
  initPatch = const $ Patch.init


instance FromToPatchState HYDRA PState StateRepr where
  loadFromPatch :: Proxy _ -> Id.FamilyR -> PState -> StateRepr -> Maybe StateRepr
  loadFromPatch _ familyR _ _ = case Id.family familyR of
    "custom" -> Just fallback
    _ -> Nothing
  putInPatch :: Proxy _ -> Id.NodeR -> StateRepr -> PState -> PState
  putInPatch _ _ _ = identity


instance HasChRepr HYDRA WrapRepr


instance PossiblyToSignature HYDRA (ValueInChannel WrapRepr) (ValueInChannel WrapRepr) Id.FamilyR where
  possiblyToSignature _ = Id.family
    >>> case _ of
        "number" -> Just $ sig "number" [] [ Sig.out_ "out" $ HYDRAW.Value (HYDRA.Number 0.0) ]
        "pi" -> Just $ sig "pi" [] [ Sig.out_ "out" $ HYDRAW.Value HYDRA.Pi ]
        "array" -> Just $ sig "array" []
          [ Sig.out_ "out" $ HYDRAW.Value (HYDRA.VArray (HYDRA.Values []) HYDRA.NoEase) ]
        "expression" -> Just $ sig "expression" [] [ Sig.out_ "out" $ HYDRAW.Value (HYDRA.Dep HYDRA.NoAction) ]
        "time" -> Just $ sig "time" [] [ Sig.out_ "time" $ HYDRAW.Value HYDRA.Time ]
        "mouse" -> Just $ sig "mouse" []
          [ Sig.out_ "x" $ HYDRAW.Value HYDRA.MouseX, Sig.out_ "y" $ HYDRAW.Value HYDRA.MouseY ]
        "glslFn" -> Just $ sig "glslFn" [] [ Sig.out_ "out" $ HYDRAW.Value (HYDRA.Dep HYDRA.NoAction) ]
        "noise" -> Just $ sig "noise"
          [ Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 10.0), Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.1) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "voronoi" -> Just $ sig "voronoi"
          [ Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 5.0)
          , Sig.in_ "speed" $ HYDRAW.Value (HYDRA.Number 0.3)
          , Sig.in_ "blending" $ HYDRAW.Value (HYDRA.Number 0.3)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "osc" -> Just $ sig "osc"
          [ Sig.in_ "frequency" $ HYDRAW.Value (HYDRA.Number 60.0)
          , Sig.in_ "sync" $ HYDRAW.Value (HYDRA.Number 0.1)
          , Sig.inx_ "offset"
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "shape" -> Just $ sig "shape"
          [ Sig.in_ "sides" $ HYDRAW.Value (HYDRA.Number 60.0)
          , Sig.in_ "radius" $ HYDRAW.Value (HYDRA.Number 0.3)
          , Sig.in_ "smoothing" $ HYDRAW.Value (HYDRA.Number 0.01)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "gradient" -> Just $ sig "gradient" [ Sig.inx_ "speed" ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "src" -> Just $ sig "src" [ Sig.inx_ "load" ] [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "solid" -> Just $ sig "solid"
          [ Sig.inx_ "r", Sig.inx_ "g", Sig.inx_ "b", Sig.in_ "a" $ HYDRAW.Value (HYDRA.Number 1.0) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "prev" -> Just $ sig "prev" [ Sig.in_ "todo" $ HYDRAW.TODO HYDRA.TODO ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "rotate" -> Just $ sig "rotate"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "angle" $ HYDRAW.Value (HYDRA.Number 10.0)
          , Sig.in_ "speed" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "scale" -> Just $ sig "scale"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.5)
          , Sig.in_ "xMult" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "yMult" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "offsetX" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "offsetY" $ HYDRAW.Value (HYDRA.Number 0.5)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "pixelate" -> Just $ sig "pixelate"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "pixelX" $ HYDRAW.Value (HYDRA.Number 20.0)
          , Sig.in_ "pixelY" $ HYDRAW.Value (HYDRA.Number 20.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "repeat" -> Just $ sig "repeat"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "repeatX" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "repeatY" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "offsetX" $ HYDRAW.Value (HYDRA.Number 0.0)
          , Sig.in_ "offsetY" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "repeatX" -> Just $ sig "repeatX"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "reps" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "repeatY" -> Just $ sig "repeatY"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "reps" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "kaleid" -> Just $ sig "kaleid"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "nSides" $ HYDRAW.Value (HYDRA.Number 3.0) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "scroll" -> Just $ sig "scroll"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scrollX" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "scrollY" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.inx_ "speedX"
          , Sig.inx_ "speedY"
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "scrollX" -> Just $ sig "scrollX"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scrollX" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "speed" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "scrollY" -> Just $ sig "scrollY"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scrollY" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "speed" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "posterize" -> Just $ sig "posterize"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "bins" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "gamma" $ HYDRAW.Value (HYDRA.Number 0.6)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "shift" -> Just $ sig "shift"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "r" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "g" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "b" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "a" $ HYDRAW.Value (HYDRA.Number 0.5)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "invert" -> Just $ sig "invert"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.0) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "contrast" -> Just $ sig "contrast"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.6) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "brightness" -> Just $ sig "brightness"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 0.4) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "luma" -> Just $ sig "luma"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "threshold" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "tolerance" $ HYDRAW.Value (HYDRA.Number 0.1)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "thresh" -> Just $ sig "thresh"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "threshold" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "tolerance" $ HYDRAW.Value (HYDRA.Number 0.1)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "color" -> Just $ sig "color"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "r" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "g" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "b" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "a" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "saturate" -> Just $ sig "saturate"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 2.0) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "hue" -> Just $ sig "hue"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "hue" $ HYDRAW.Value (HYDRA.Number 0.4) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "colorama" -> Just $ sig "colorama"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 0.005) ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "sum" -> Just $ sig "sum"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "todo" $ HYDRAW.TODO HYDRA.TODO ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "r" -> Just $ sig "r"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "b" -> Just $ sig "b"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "g" -> Just $ sig "g"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "a" -> Just $ sig "a"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "add" -> Just $ sig "add"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "sub" -> Just $ sig "sub"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "layer" -> Just $ sig "layer"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "blend" -> Just $ sig "blend"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 0.5)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "mult" -> Just $ sig "mult"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "diff" -> Just $ sig "diff"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "mask" -> Just $ sig "mask"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateRepeat" -> Just $ sig "modulateRepeat"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "repeatX" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "repeatY" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "offsetX" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.in_ "offsetY" $ HYDRAW.Value (HYDRA.Number 0.5)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateRepeatX" -> Just $ sig "modulateRepeatX"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "reps" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.5)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateRepeatY" -> Just $ sig "modulateRepeatY"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "reps" $ HYDRAW.Value (HYDRA.Number 3.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.5)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateKaleid" -> Just $ sig "modulateKaleid"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "nSides" $ HYDRAW.Value (HYDRA.Number 3.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateScrollX" -> Just $ sig "modulateScrollX"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scrollX" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.inx_ "speed"
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateScrollY" -> Just $ sig "modulateScrollY"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "scrollY" $ HYDRAW.Value (HYDRA.Number 0.5)
          , Sig.inx_ "speed"
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulate" -> Just $ sig "modulate"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 0.1)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateScale" -> Just $ sig "modulateScale"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "multiple" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulatePixelate" -> Just $ sig "modulatePixelate"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "multiple" $ HYDRAW.Value (HYDRA.Number 10.0)
          , Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 3.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateRotate" -> Just $ sig "modulateRotate"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "multiple" $ HYDRAW.Value (HYDRA.Number 1.0)
          , Sig.inx_ "offset"
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "modulateHue" -> Just $ sig "modulateHue"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "with" $ HYDRAW.Texture HYDRA.Empty
          , Sig.in_ "amount" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "out" $ HYDRAW.Texture HYDRA.Empty ]
        "initCam" -> Just $ sig "initCam" [ Sig.inx_ "src", Sig.in_ "index" $ HYDRAW.Value HYDRA.None ] []
        "initImage" -> Just $ sig "initImage" [ Sig.inx_ "src", Sig.inx_ "url" ] []
        "initVideo" -> Just $ sig "initVideo" [ Sig.inx_ "src", Sig.inx_ "url" ] []
        "init" -> Just $ sig "init" [ Sig.inx_ "options" ] []
        "initStream" -> Just $ sig "initStream" [ Sig.inx_ "src", Sig.in_ "todo" $ HYDRAW.TODO HYDRA.TODO ]
          []
        "initScreen" -> Just $ sig "initScreen" [] []
        "render" -> Just $ sig "render" [ Sig.in_ "what" $ HYDRAW.Unit unit ] []
        "update" -> Just $ sig "update" [ Sig.inx_ "fn" ] []
        "setResolution" -> Just $ sig "setResolution"
          [ Sig.in_ "width" $ HYDRAW.Value (HYDRA.Number 100.0)
          , Sig.in_ "height" $ HYDRAW.Value (HYDRA.Number 100.0)
          ]
          []
        "hush" -> Just $ sig "hush" [] []
        "setFunction" -> Just $ sig "setFunction" [ Sig.inx_ "fn" ] []
        "speed" -> Just $ sig "speed" [ Sig.in_ "v" $ HYDRAW.Value (HYDRA.Number 1.0) ] []
        "bpm" -> Just $ sig "bpm" [ Sig.in_ "v" $ HYDRAW.Value (HYDRA.Number 30.0) ] []
        "width" -> Just $ sig "width" [] [ Sig.out_ "w" $ HYDRAW.Value HYDRA.Width ]
        "height" -> Just $ sig "height" [] [ Sig.out_ "h" $ HYDRAW.Value HYDRA.Height ]
        "fast" -> Just $ sig "fast"
          [ Sig.in_ "arr" $ HYDRAW.Values (HYDRA.Values []), Sig.in_ "speed" $ HYDRAW.Value (HYDRA.Number 1.0) ]
          [ Sig.out_ "arr" $ HYDRAW.Value (HYDRA.VArray (HYDRA.Values []) HYDRA.NoEase) ]
        "smooth" -> Just $ sig "smooth"
          [ Sig.in_ "arr" $ HYDRAW.Values (HYDRA.Values []), Sig.in_ "smooth" $ HYDRAW.Value (HYDRA.Number 1.0) ]
          [ Sig.out_ "arr" $ HYDRAW.Value (HYDRA.VArray (HYDRA.Values []) HYDRA.NoEase) ]
        "ease" -> Just $ sig "ease"
          [ Sig.in_ "arr" $ HYDRAW.Values (HYDRA.Values []), Sig.in_ "ease" $ HYDRAW.Ease HYDRA.NoEase ]
          [ Sig.out_ "arr" $ HYDRAW.Value (HYDRA.VArray (HYDRA.Values []) HYDRA.NoEase ) ]
        "offset" -> Just $ sig "offset"
          [ Sig.in_ "arr" $ HYDRAW.Values (HYDRA.Values []), Sig.in_ "offset" $ HYDRAW.Value (HYDRA.Number 0.5) ]
          [ Sig.out_ "arr" $ HYDRAW.Value (HYDRA.VArray (HYDRA.Values []) HYDRA.NoEase) ]
        "fit" -> Just $ sig "fit"
          [ Sig.in_ "arr" $ HYDRAW.Values (HYDRA.Values [])
          , Sig.in_ "low" $ HYDRAW.Value (HYDRA.Number 0.0)
          , Sig.in_ "high" $ HYDRAW.Value (HYDRA.Number 1.0)
          ]
          [ Sig.out_ "arr" $ HYDRAW.Value (HYDRA.VArray (HYDRA.Values []) HYDRA.NoEase) ]
        "fft" -> Just $ sig "fft" [ Sig.in_ "bin" $ HYDRAW.AudioBin (HYDRA.AudioBin 0) ]
          [ Sig.out_ "fft" $ HYDRAW.Value HYDRA.None ]
        "setSmooth" -> Just $ sig "setSmooth"
          [ Sig.in_ "audio" $ HYDRAW.Audio HYDRA.Silence, Sig.in_ "smooth" $ HYDRAW.Value (HYDRA.Number 0.4) ]
          []
        "setCutoff" -> Just $ sig "setCutoff"
          [ Sig.in_ "audio" $ HYDRAW.Audio HYDRA.Silence, Sig.in_ "cutoff" $ HYDRAW.Value (HYDRA.Number 2.0) ]
          []
        "setBins" -> Just $ sig "setBins"
          [ Sig.in_ "audio" $ HYDRAW.Audio HYDRA.Silence, Sig.in_ "numBins" $ HYDRAW.Value (HYDRA.Number 4.0) ]
          []
        "setScale" -> Just $ sig "setScale"
          [ Sig.in_ "audio" $ HYDRAW.Audio HYDRA.Silence, Sig.in_ "scale" $ HYDRAW.Value (HYDRA.Number 10.0) ]
          []
        "hide" -> Just $ sig "hide"
          [ Sig.in_ "audio" $ HYDRAW.Audio HYDRA.Silence, Sig.in_ "todo" $ HYDRAW.TODO HYDRA.TODO ]
          []
        "show" -> Just $ sig "show"
          [ Sig.in_ "audio" $ HYDRAW.Audio HYDRA.Silence, Sig.in_ "todo" $ HYDRAW.TODO HYDRA.TODO ]
          []
        "out" -> Just $ sig "out"
          [ Sig.in_ "what" $ HYDRAW.Texture HYDRA.Empty, Sig.in_ "target" $ HYDRAW.Unit unit ]
          []
        _ -> Nothing
    >>> map Sig.toChanneled


instance MonadEffect m => WebEditor HYDRA WrapRepr m where
  -- webEditorFor :: Proxy HYDRA -> WR.InletPath -> ValueInChannel WrapRepr -> Maybe ValueEditor.EditorId
  -- webEditorFor _ _ _ = Nothing
  spawnWebEditor :: Proxy HYDRA -> {- H.RefLabel -> -} ValueEditor.EditorId -> WR.InletPath -> ValueInChannel WrapRepr -> Maybe (ValueEditor WrapRepr m)
  spawnWebEditor _ {- _ -} _ _ _ =
    Just $ NumericVE.editor toNumber fromNumber
    where
      toNumber = case _ of -- FIXME: use some typeclass
        HYDRAW.Value (HYDRA.Number n) -> Just n
        _ -> Nothing
      fromNumber = HYDRAW.Value <<< HYDRA.Number