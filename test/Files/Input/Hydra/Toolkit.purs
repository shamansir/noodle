module INPUT.Hydra.Gen.Toolkit where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Color as Color
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Noodle.Id (toolkitR, family, FamilyR, unsafeGroupR, group, NodeR) as Id
import Noodle.Fn.Signature (sig, class PossiblyToSignature)
import Noodle.Fn.Signature (in_, inx_, out_, outx_, toChanneled) as Sig
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasChRepr, class InitPatchState, class FromToPatchState, markGroup)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.HasFallback (fallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Cli.Class.CliRenderer (class CliRenderer, class CliRawRenderer, class CliEditor)
import Web.Class.WebRenderer (class WebRenderer, class WebRawRenderer, class WebEditor)
import Web.Class.WebRenderer (InletPath) as WR
import Web.Components.ValueEditor (ValueEditor)
import Web.Components.ValueEditor (EditorId) as ValueEditor
import Web.Components.ValueEditor.Numeric as NumericVE
import Halogen (Component, RefLabel) as H
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.Number as Feed.Number
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.Pi as Feed.Pi
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.Array as Feed.Array
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.Expression as Feed.Expression
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.Time as Feed.Time
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.Mouse as Feed.Mouse
import INPUT.Test.Files.CodeGenTest.Hydra.Feed.GlslFn as Feed.GlslFn
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Noise as Source.Noise
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Voronoi as Source.Voronoi
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Osc as Source.Osc
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Shape as Source.Shape
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Gradient as Source.Gradient
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Src as Source.Src
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Solid as Source.Solid
import INPUT.Test.Files.CodeGenTest.Hydra.Source.Prev as Source.Prev
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.Rotate as Geometry.Rotate
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.Scale as Geometry.Scale
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.Pixelate as Geometry.Pixelate
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.Repeat as Geometry.Repeat
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.RepeatX as Geometry.RepeatX
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.RepeatY as Geometry.RepeatY
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.Kaleid as Geometry.Kaleid
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.Scroll as Geometry.Scroll
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.ScrollX as Geometry.ScrollX
import INPUT.Test.Files.CodeGenTest.Hydra.Geometry.ScrollY as Geometry.ScrollY
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Posterize as Color.Posterize
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Shift as Color.Shift
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Invert as Color.Invert
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Contrast as Color.Contrast
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Brightness as Color.Brightness
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Luma as Color.Luma
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Thresh as Color.Thresh
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Color as Color.Color
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Saturate as Color.Saturate
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Hue as Color.Hue
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Colorama as Color.Colorama
import INPUT.Test.Files.CodeGenTest.Hydra.Color.Sum as Color.Sum
import INPUT.Test.Files.CodeGenTest.Hydra.Color.R as Color.R
import INPUT.Test.Files.CodeGenTest.Hydra.Color.B as Color.B
import INPUT.Test.Files.CodeGenTest.Hydra.Color.G as Color.G
import INPUT.Test.Files.CodeGenTest.Hydra.Color.A as Color.A
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Add as Blend.Add
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Sub as Blend.Sub
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Layer as Blend.Layer
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Blend as Blend.Blend
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Mult as Blend.Mult
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Diff as Blend.Diff
import INPUT.Test.Files.CodeGenTest.Hydra.Blend.Mask as Blend.Mask
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateRepeat as Modulate.ModulateRepeat
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateRepeatX as Modulate.ModulateRepeatX
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateRepeatY as Modulate.ModulateRepeatY
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateKaleid as Modulate.ModulateKaleid
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateScrollX as Modulate.ModulateScrollX
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateScrollY as Modulate.ModulateScrollY
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.Modulate as Modulate.Modulate
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateScale as Modulate.ModulateScale
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulatePixelate as Modulate.ModulatePixelate
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateRotate as Modulate.ModulateRotate
import INPUT.Test.Files.CodeGenTest.Hydra.Modulate.ModulateHue as Modulate.ModulateHue
import INPUT.Test.Files.CodeGenTest.Hydra.Extsource.InitCam as Extsource.InitCam
import INPUT.Test.Files.CodeGenTest.Hydra.Extsource.InitImage as Extsource.InitImage
import INPUT.Test.Files.CodeGenTest.Hydra.Extsource.InitVideo as Extsource.InitVideo
import INPUT.Test.Files.CodeGenTest.Hydra.Extsource.Init as Extsource.Init
import INPUT.Test.Files.CodeGenTest.Hydra.Extsource.InitStream as Extsource.InitStream
import INPUT.Test.Files.CodeGenTest.Hydra.Extsource.InitScreen as Extsource.InitScreen
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Render as Synth.Render
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Update as Synth.Update
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.SetResolution as Synth.SetResolution
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Hush as Synth.Hush
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.SetFunction as Synth.SetFunction
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Speed as Synth.Speed
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Bpm as Synth.Bpm
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Width as Synth.Width
import INPUT.Test.Files.CodeGenTest.Hydra.Synth.Height as Synth.Height
import INPUT.Test.Files.CodeGenTest.Hydra.Array.Fast as Array.Fast
import INPUT.Test.Files.CodeGenTest.Hydra.Array.Smooth as Array.Smooth
import INPUT.Test.Files.CodeGenTest.Hydra.Array.Ease as Array.Ease
import INPUT.Test.Files.CodeGenTest.Hydra.Array.Offset as Array.Offset
import INPUT.Test.Files.CodeGenTest.Hydra.Array.Fit as Array.Fit
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.Fft as Audio.Fft
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.SetSmooth as Audio.SetSmooth
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.SetCutoff as Audio.SetCutoff
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.SetBins as Audio.SetBins
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.SetScale as Audio.SetScale
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.Hide as Audio.Hide
import INPUT.Test.Files.CodeGenTest.Hydra.Audio.Show as Audio.Show
import INPUT.Test.Files.CodeGenTest.Hydra.Out.Out as Out.Out
import HydraTk.Repr.State (StateRepr)
import HydraTk.Repr.Wrap (WrapRepr)
import HydraTk.Patch (PState(..))
import HydraTk.Patch (init) as Patch
import HydraTk.Types as HYDRA
import HydraTk.Repr.Wrap as HYDRAW
import Data.Tuple.Nested ((/\))

type HydraFamilies :: Families
type HydraFamilies = Feed.Number.F :> Feed.Pi.F :> Feed.Array.F :> Feed.Expression.F :> Feed.Time.F
  :> Feed.Mouse.F
  :> Feed.GlslFn.F
  :> Source.Noise.F
  :> Source.Voronoi.F
  :> Source.Osc.F
  :> Source.Shape.F
  :> Source.Gradient.F
  :> Source.Src.F
  :> Source.Solid.F
  :> Source.Prev.F
  :> Geometry.Rotate.F
  :> Geometry.Scale.F
  :> Geometry.Pixelate.F
  :> Geometry.Repeat.F
  :> Geometry.RepeatX.F
  :> Geometry.RepeatY.F
  :> Geometry.Kaleid.F
  :> Geometry.Scroll.F
  :> Geometry.ScrollX.F
  :> Geometry.ScrollY.F
  :> Color.Posterize.F
  :> Color.Shift.F
  :> Color.Invert.F
  :> Color.Contrast.F
  :> Color.Brightness.F
  :> Color.Luma.F
  :> Color.Thresh.F
  :> Color.Color.F
  :> Color.Saturate.F
  :> Color.Hue.F
  :> Color.Colorama.F
  :> Color.Sum.F
  :> Color.R.F
  :> Color.B.F
  :> Color.G.F
  :> Color.A.F
  :> Blend.Add.F
  :> Blend.Sub.F
  :> Blend.Layer.F
  :> Blend.Blend.F
  :> Blend.Mult.F
  :> Blend.Diff.F
  :> Blend.Mask.F
  :> Modulate.ModulateRepeat.F
  :> Modulate.ModulateRepeatX.F
  :> Modulate.ModulateRepeatY.F
  :> Modulate.ModulateKaleid.F
  :> Modulate.ModulateScrollX.F
  :> Modulate.ModulateScrollY.F
  :> Modulate.Modulate.F
  :> Modulate.ModulateScale.F
  :> Modulate.ModulatePixelate.F
  :> Modulate.ModulateRotate.F
  :> Modulate.ModulateHue.F
  :> Extsource.InitCam.F
  :> Extsource.InitImage.F
  :> Extsource.InitVideo.F
  :> Extsource.Init.F
  :> Extsource.InitStream.F
  :> Extsource.InitScreen.F
  :> Synth.Render.F
  :> Synth.Update.F
  :> Synth.SetResolution.F
  :> Synth.Hush.F
  :> Synth.SetFunction.F
  :> Synth.Speed.F
  :> Synth.Bpm.F
  :> Synth.Width.F
  :> Synth.Height.F
  :> Array.Fast.F
  :> Array.Smooth.F
  :> Array.Ease.F
  :> Array.Offset.F
  :> Array.Fit.F
  :> Audio.Fft.F
  :> Audio.SetSmooth.F
  :> Audio.SetCutoff.F
  :> Audio.SetBins.F
  :> Audio.SetScale.F
  :> Audio.Hide.F
  :> Audio.Show.F
  :> Out.Out.F
  :> TNil

foreign import data HYDRA :: ToolkitKey

toolkit :: Toolkit HYDRA HydraFamilies StateRepr WrapRepr Effect
toolkit = Toolkit.register Feed.Number.family $ Toolkit.register Feed.Pi.family
  $ Toolkit.register Feed.Array.family
  $ Toolkit.register Feed.Expression.family
  $ Toolkit.register Feed.Time.family
  $ Toolkit.register Feed.Mouse.family
  $ Toolkit.register Feed.GlslFn.family
  $ Toolkit.register Source.Noise.family
  $ Toolkit.register Source.Voronoi.family
  $ Toolkit.register Source.Osc.family
  $ Toolkit.register Source.Shape.family
  $ Toolkit.register Source.Gradient.family
  $ Toolkit.register Source.Src.family
  $ Toolkit.register Source.Solid.family
  $ Toolkit.register Source.Prev.family
  $ Toolkit.register Geometry.Rotate.family
  $ Toolkit.register Geometry.Scale.family
  $ Toolkit.register Geometry.Pixelate.family
  $ Toolkit.register Geometry.Repeat.family
  $ Toolkit.register Geometry.RepeatX.family
  $ Toolkit.register Geometry.RepeatY.family
  $ Toolkit.register Geometry.Kaleid.family
  $ Toolkit.register Geometry.Scroll.family
  $ Toolkit.register Geometry.ScrollX.family
  $ Toolkit.register Geometry.ScrollY.family
  $ Toolkit.register Color.Posterize.family
  $ Toolkit.register Color.Shift.family
  $ Toolkit.register Color.Invert.family
  $ Toolkit.register Color.Contrast.family
  $ Toolkit.register Color.Brightness.family
  $ Toolkit.register Color.Luma.family
  $ Toolkit.register Color.Thresh.family
  $ Toolkit.register Color.Color.family
  $ Toolkit.register Color.Saturate.family
  $ Toolkit.register Color.Hue.family
  $ Toolkit.register Color.Colorama.family
  $ Toolkit.register Color.Sum.family
  $ Toolkit.register Color.R.family
  $ Toolkit.register Color.B.family
  $ Toolkit.register Color.G.family
  $ Toolkit.register Color.A.family
  $ Toolkit.register Blend.Add.family
  $ Toolkit.register Blend.Sub.family
  $ Toolkit.register Blend.Layer.family
  $ Toolkit.register Blend.Blend.family
  $ Toolkit.register Blend.Mult.family
  $ Toolkit.register Blend.Diff.family
  $ Toolkit.register Blend.Mask.family
  $ Toolkit.register Modulate.ModulateRepeat.family
  $ Toolkit.register Modulate.ModulateRepeatX.family
  $ Toolkit.register Modulate.ModulateRepeatY.family
  $ Toolkit.register Modulate.ModulateKaleid.family
  $ Toolkit.register Modulate.ModulateScrollX.family
  $ Toolkit.register Modulate.ModulateScrollY.family
  $ Toolkit.register Modulate.Modulate.family
  $ Toolkit.register Modulate.ModulateScale.family
  $ Toolkit.register Modulate.ModulatePixelate.family
  $ Toolkit.register Modulate.ModulateRotate.family
  $ Toolkit.register Modulate.ModulateHue.family
  $ Toolkit.register Extsource.InitCam.family
  $ Toolkit.register Extsource.InitImage.family
  $ Toolkit.register Extsource.InitVideo.family
  $ Toolkit.register Extsource.Init.family
  $ Toolkit.register Extsource.InitStream.family
  $ Toolkit.register Extsource.InitScreen.family
  $ Toolkit.register Synth.Render.family
  $ Toolkit.register Synth.Update.family
  $ Toolkit.register Synth.SetResolution.family
  $ Toolkit.register Synth.Hush.family
  $ Toolkit.register Synth.SetFunction.family
  $ Toolkit.register Synth.Speed.family
  $ Toolkit.register Synth.Bpm.family
  $ Toolkit.register Synth.Width.family
  $ Toolkit.register Synth.Height.family
  $ Toolkit.register Array.Fast.family
  $ Toolkit.register Array.Smooth.family
  $ Toolkit.register Array.Ease.family
  $ Toolkit.register Array.Offset.family
  $ Toolkit.register Array.Fit.family
  $ Toolkit.register Audio.Fft.family
  $ Toolkit.register Audio.SetSmooth.family
  $ Toolkit.register Audio.SetCutoff.family
  $ Toolkit.register Audio.SetBins.family
  $ Toolkit.register Audio.SetScale.family
  $ Toolkit.register Audio.Hide.family
  $ Toolkit.register Audio.Show.family
  $ Toolkit.register Out.Out.family
  $ Toolkit.empty (Proxy :: _ HYDRA) (Id.toolkitR "Hydra")

instance HasChRepr HYDRA WrapRepr
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

instance MonadEffect m => CliRenderer HYDRA HydraFamilies WrapRepr m where
  cliSize _ _ _ _ _ = Nothing
  renderCli _ _ _ _ _ = Nothing

instance MonadEffect m => CliRawRenderer HYDRA HydraFamilies WrapRepr m where
  cliSizeRaw _ _ _ _ _ = Nothing
  renderCliRaw _ _ _ _ _ = Nothing

instance CliEditor HYDRA WrapRepr where
  cliEditorFor _ _ _ _ _ _ = Nothing

instance MonadEffect m => WebEditor HYDRA WrapRepr m where
  spawnWebEditor :: Proxy HYDRA -> ValueEditor.EditorId -> WR.InletPath -> ValueInChannel WrapRepr -> Maybe (ValueEditor WrapRepr m)
  spawnWebEditor _ _ _ _ =
    Just $ NumericVE.editor toNumber fromNumber
    where
      toNumber = case _ of
        HYDRAW.Value (HYDRA.Number n) -> Just n
        _ -> Nothing
      fromNumber = HYDRAW.Value <<< HYDRA.Number

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
