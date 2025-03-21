module HydraTk.Toolkit where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Color as Color
import Data.Maybe (Maybe(..))
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Noodle.Id (toolkitR, family, FamilyR, unsafeGroupR, group) as Id
import Noodle.Fn.Signature (sig, class PossiblyToSignature)
import Noodle.Fn.Signature (in_, inx_, out_, outx_, toChanneled) as Sig
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasChRepr, class FromPatchState, markGroup)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.HasFallback (fallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Cli.Class.CliRenderer (class CliRenderer, class CliRawRenderer, class CliEditor)
import HydraTk.Library.Feed.Number as Feed.Number
import HydraTk.Library.Feed.Pi as Feed.Pi
import HydraTk.Library.Feed.Array as Feed.Array
import HydraTk.Library.Feed.Expression as Feed.Expression
import HydraTk.Library.Feed.Time as Feed.Time
import HydraTk.Library.Feed.Mouse as Feed.Mouse
import HydraTk.Library.Feed.GlslFn as Feed.GlslFn
import HydraTk.Library.Source.Noise as Source.Noise
import HydraTk.Library.Source.Voronoi as Source.Voronoi
import HydraTk.Library.Source.Osc as Source.Osc
import HydraTk.Library.Source.Shape as Source.Shape
import HydraTk.Library.Source.Gradient as Source.Gradient
import HydraTk.Library.Source.Src as Source.Src
import HydraTk.Library.Source.Solid as Source.Solid
import HydraTk.Library.Source.Prev as Source.Prev
import HydraTk.Library.Geometry.Rotate as Geometry.Rotate
import HydraTk.Library.Geometry.Scale as Geometry.Scale
import HydraTk.Library.Geometry.Pixelate as Geometry.Pixelate
import HydraTk.Library.Geometry.Repeat as Geometry.Repeat
import HydraTk.Library.Geometry.RepeatX as Geometry.RepeatX
import HydraTk.Library.Geometry.RepeatY as Geometry.RepeatY
import HydraTk.Library.Geometry.Kaleid as Geometry.Kaleid
import HydraTk.Library.Geometry.Scroll as Geometry.Scroll
import HydraTk.Library.Geometry.ScrollX as Geometry.ScrollX
import HydraTk.Library.Geometry.ScrollY as Geometry.ScrollY
import HydraTk.Library.Color.Posterize as Color.Posterize
import HydraTk.Library.Color.Shift as Color.Shift
import HydraTk.Library.Color.Invert as Color.Invert
import HydraTk.Library.Color.Contrast as Color.Contrast
import HydraTk.Library.Color.Brightness as Color.Brightness
import HydraTk.Library.Color.Luma as Color.Luma
import HydraTk.Library.Color.Thresh as Color.Thresh
import HydraTk.Library.Color.Color as Color.Color
import HydraTk.Library.Color.Saturate as Color.Saturate
import HydraTk.Library.Color.Hue as Color.Hue
import HydraTk.Library.Color.Colorama as Color.Colorama
import HydraTk.Library.Color.Sum as Color.Sum
import HydraTk.Library.Color.R as Color.R
import HydraTk.Library.Color.B as Color.B
import HydraTk.Library.Color.G as Color.G
import HydraTk.Library.Color.A as Color.A
import HydraTk.Library.Blend.Add as Blend.Add
import HydraTk.Library.Blend.Sub as Blend.Sub
import HydraTk.Library.Blend.Layer as Blend.Layer
import HydraTk.Library.Blend.Blend as Blend.Blend
import HydraTk.Library.Blend.Mult as Blend.Mult
import HydraTk.Library.Blend.Diff as Blend.Diff
import HydraTk.Library.Blend.Mask as Blend.Mask
import HydraTk.Library.Modulate.ModulateRepeat as Modulate.ModulateRepeat
import HydraTk.Library.Modulate.ModulateRepeatX as Modulate.ModulateRepeatX
import HydraTk.Library.Modulate.ModulateRepeatY as Modulate.ModulateRepeatY
import HydraTk.Library.Modulate.ModulateKaleid as Modulate.ModulateKaleid
import HydraTk.Library.Modulate.ModulateScrollX as Modulate.ModulateScrollX
import HydraTk.Library.Modulate.ModulateScrollY as Modulate.ModulateScrollY
import HydraTk.Library.Modulate.Modulate as Modulate.Modulate
import HydraTk.Library.Modulate.ModulateScale as Modulate.ModulateScale
import HydraTk.Library.Modulate.ModulatePixelate as Modulate.ModulatePixelate
import HydraTk.Library.Modulate.ModulateRotate as Modulate.ModulateRotate
import HydraTk.Library.Modulate.ModulateHue as Modulate.ModulateHue
import HydraTk.Library.Extsource.InitCam as Extsource.InitCam
import HydraTk.Library.Extsource.InitImage as Extsource.InitImage
import HydraTk.Library.Extsource.InitVideo as Extsource.InitVideo
import HydraTk.Library.Extsource.Init as Extsource.Init
import HydraTk.Library.Extsource.InitStream as Extsource.InitStream
import HydraTk.Library.Extsource.InitScreen as Extsource.InitScreen
import HydraTk.Library.Synth.Render as Synth.Render
import HydraTk.Library.Synth.Update as Synth.Update
import HydraTk.Library.Synth.SetResolution as Synth.SetResolution
import HydraTk.Library.Synth.Hush as Synth.Hush
import HydraTk.Library.Synth.SetFunction as Synth.SetFunction
import HydraTk.Library.Synth.Speed as Synth.Speed
import HydraTk.Library.Synth.Bpm as Synth.Bpm
import HydraTk.Library.Synth.Width as Synth.Width
import HydraTk.Library.Synth.Height as Synth.Height
import HydraTk.Library.Array.Fast as Array.Fast
import HydraTk.Library.Array.Smooth as Array.Smooth
import HydraTk.Library.Array.Ease as Array.Ease
import HydraTk.Library.Array.Offset as Array.Offset
import HydraTk.Library.Array.Fit as Array.Fit
import HydraTk.Library.Audio.Fft as Audio.Fft
import HydraTk.Library.Audio.SetSmooth as Audio.SetSmooth
import HydraTk.Library.Audio.SetCutoff as Audio.SetCutoff
import HydraTk.Library.Audio.SetBins as Audio.SetBins
import HydraTk.Library.Audio.SetScale as Audio.SetScale
import HydraTk.Library.Audio.Hide as Audio.Hide
import HydraTk.Library.Audio.Show as Audio.Show
import HydraTk.Library.Out.Out as Out.Out
import HydraTk.Patch (PState)
import HydraTk.Repr.Wrap (WrapRepr)
import HydraTk.Repr.State (StateRepr)
import HydraTk.Types as HT
import HydraTk.Repr.Wrap as HW
import Data.Tuple.Nested ((/\))

type HydraFamilies :: Families
type HydraFamilies
  =  Feed.Number.F
  :> Feed.Pi.F
  :> Feed.Array.F
  :> Feed.Expression.F
  :> Feed.Time.F
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
toolkit
  = Toolkit.register Feed.Number.family
  $ Toolkit.register Feed.Pi.family
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
  editorFor _ _ _ _ _ _ = Nothing

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

instance FromPatchState HYDRA PState StateRepr where
  loadFromPatch :: Proxy _ -> Id.FamilyR -> PState -> Maybe StateRepr
  loadFromPatch _ familyR _ = case Id.family familyR of
    "custom" -> Just fallback
    _ -> Nothing

instance PossiblyToSignature HYDRA (ValueInChannel WrapRepr) (ValueInChannel WrapRepr) Id.FamilyR where
  possiblyToSignature _ = Id.family
    >>> case _ of
        "number" -> Just $ sig "number" [] [ Sig.out_ "out" $ HW.Value (HT.Number 0.0) ]
        "pi" -> Just $ sig "pi" [] [ Sig.out_ "out" $ HW.Value HT.Pi ]
        "array" -> Just $ sig "array" []
          [ Sig.out_ "out" $ HW.Value (HT.VArray (HT.Values []) HT.Linear) ]
        "expression" -> Just $ sig "expression" [] [ Sig.out_ "out" $ HW.Value (HT.Dep HT.NoAction) ]
        "time" -> Just $ sig "time" [] [ Sig.out_ "time" $ HW.Value HT.Time ]
        "mouse" -> Just $ sig "mouse" []
          [ Sig.out_ "x" $ HW.Value HT.MouseX, Sig.out_ "y" $ HW.Value HT.MouseY ]
        "glslFn" -> Just $ sig "glslFn" [] [ Sig.out_ "out" $ HW.Value (HT.Dep HT.NoAction) ]
        "noise" -> Just $ sig "noise"
          [ Sig.in_ "scale" $ HW.Value (HT.Number 10.0), Sig.in_ "offset" $ HW.Value (HT.Number 0.1) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "voronoi" -> Just $ sig "voronoi"
          [ Sig.in_ "scale" $ HW.Value (HT.Number 5.0)
          , Sig.in_ "speed" $ HW.Value (HT.Number 0.3)
          , Sig.in_ "blending" $ HW.Value (HT.Number 0.3)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "osc" -> Just $ sig "osc"
          [ Sig.in_ "frequency" $ HW.Value (HT.Number 60.0)
          , Sig.in_ "sync" $ HW.Value (HT.Number 0.1)
          , Sig.inx_ "offset"
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "shape" -> Just $ sig "shape"
          [ Sig.in_ "sides" $ HW.Value (HT.Number 60.0)
          , Sig.in_ "radius" $ HW.Value (HT.Number 0.3)
          , Sig.in_ "smoothing" $ HW.Value (HT.Number 0.01)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "gradient" -> Just $ sig "gradient" [ Sig.inx_ "speed" ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "src" -> Just $ sig "src" [ Sig.inx_ "load" ] [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "solid" -> Just $ sig "solid"
          [ Sig.inx_ "r", Sig.inx_ "g", Sig.inx_ "b", Sig.in_ "a" $ HW.Value (HT.Number 1.0) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "prev" -> Just $ sig "prev" [ Sig.in_ "todo" $ HW.TODO HT.TODO ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "rotate" -> Just $ sig "rotate"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "angle" $ HW.Value (HT.Number 10.0)
          , Sig.in_ "speed" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "scale" -> Just $ sig "scale"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 1.5)
          , Sig.in_ "xMult" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "yMult" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "offsetX" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "offsetY" $ HW.Value (HT.Number 0.5)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "pixelate" -> Just $ sig "pixelate"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "pixelX" $ HW.Value (HT.Number 20.0)
          , Sig.in_ "pixelY" $ HW.Value (HT.Number 20.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "repeat" -> Just $ sig "repeat"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "repeatX" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "repeatY" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "offsetX" $ HW.Value (HT.Number 0.0)
          , Sig.in_ "offsetY" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "repeatX" -> Just $ sig "repeatX"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "reps" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "repeatY" -> Just $ sig "repeatY"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "reps" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "kaleid" -> Just $ sig "kaleid"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "nSides" $ HW.Value (HT.Number 3.0) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "scroll" -> Just $ sig "scroll"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scrollX" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "scrollY" $ HW.Value (HT.Number 0.5)
          , Sig.inx_ "speedX"
          , Sig.inx_ "speedY"
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "scrollX" -> Just $ sig "scrollX"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scrollX" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "speed" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "scrollY" -> Just $ sig "scrollY"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scrollY" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "speed" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "posterize" -> Just $ sig "posterize"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "bins" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "gamma" $ HW.Value (HT.Number 0.6)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "shift" -> Just $ sig "shift"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "r" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "g" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "b" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "a" $ HW.Value (HT.Number 0.5)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "invert" -> Just $ sig "invert"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "amount" $ HW.Value (HT.Number 1.0) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "contrast" -> Just $ sig "contrast"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "amount" $ HW.Value (HT.Number 1.6) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "brightness" -> Just $ sig "brightness"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "amount" $ HW.Value (HT.Number 0.4) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "luma" -> Just $ sig "luma"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "threshold" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "tolerance" $ HW.Value (HT.Number 0.1)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "thresh" -> Just $ sig "thresh"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "threshold" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "tolerance" $ HW.Value (HT.Number 0.1)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "color" -> Just $ sig "color"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "r" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "g" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "b" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "a" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "saturate" -> Just $ sig "saturate"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "amount" $ HW.Value (HT.Number 2.0) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "hue" -> Just $ sig "hue"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "hue" $ HW.Value (HT.Number 0.4) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "colorama" -> Just $ sig "colorama"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "amount" $ HW.Value (HT.Number 0.005) ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "sum" -> Just $ sig "sum"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "todo" $ HW.TODO HT.TODO ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "r" -> Just $ sig "r"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scale" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "b" -> Just $ sig "b"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scale" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "g" -> Just $ sig "g"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scale" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "a" -> Just $ sig "a"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "scale" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "add" -> Just $ sig "add"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "sub" -> Just $ sig "sub"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "layer" -> Just $ sig "layer"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "blend" -> Just $ sig "blend"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 0.5)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "mult" -> Just $ sig "mult"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "diff" -> Just $ sig "diff"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "with" $ HW.Texture HT.Empty ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "mask" -> Just $ sig "mask"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "with" $ HW.Texture HT.Empty ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateRepeat" -> Just $ sig "modulateRepeat"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "repeatX" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "repeatY" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "offsetX" $ HW.Value (HT.Number 0.5)
          , Sig.in_ "offsetY" $ HW.Value (HT.Number 0.5)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateRepeatX" -> Just $ sig "modulateRepeatX"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "reps" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.5)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateRepeatY" -> Just $ sig "modulateRepeatY"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "reps" $ HW.Value (HT.Number 3.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 0.5)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateKaleid" -> Just $ sig "modulateKaleid"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "nSides" $ HW.Value (HT.Number 3.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateScrollX" -> Just $ sig "modulateScrollX"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "scrollX" $ HW.Value (HT.Number 0.5)
          , Sig.inx_ "speed"
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateScrollY" -> Just $ sig "modulateScrollY"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "scrollY" $ HW.Value (HT.Number 0.5)
          , Sig.inx_ "speed"
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulate" -> Just $ sig "modulate"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 0.1)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateScale" -> Just $ sig "modulateScale"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "multiple" $ HW.Value (HT.Number 1.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulatePixelate" -> Just $ sig "modulatePixelate"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "multiple" $ HW.Value (HT.Number 10.0)
          , Sig.in_ "offset" $ HW.Value (HT.Number 3.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateRotate" -> Just $ sig "modulateRotate"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "multiple" $ HW.Value (HT.Number 1.0)
          , Sig.inx_ "offset"
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "modulateHue" -> Just $ sig "modulateHue"
          [ Sig.in_ "what" $ HW.Texture HT.Empty
          , Sig.in_ "with" $ HW.Texture HT.Empty
          , Sig.in_ "amount" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "out" $ HW.Texture HT.Empty ]
        "initCam" -> Just $ sig "initCam" [ Sig.inx_ "src", Sig.in_ "index" $ HW.Value HT.None ] []
        "initImage" -> Just $ sig "initImage" [ Sig.inx_ "src", Sig.inx_ "url" ] []
        "initVideo" -> Just $ sig "initVideo" [ Sig.inx_ "src", Sig.inx_ "url" ] []
        "init" -> Just $ sig "init" [ Sig.inx_ "options" ] []
        "initStream" -> Just $ sig "initStream" [ Sig.inx_ "src", Sig.in_ "todo" $ HW.TODO HT.TODO ]
          []
        "initScreen" -> Just $ sig "initScreen" [] []
        "render" -> Just $ sig "render" [ Sig.in_ "what" $ HW.Unit unit ] []
        "update" -> Just $ sig "update" [ Sig.inx_ "fn" ] []
        "setResolution" -> Just $ sig "setResolution"
          [ Sig.in_ "width" $ HW.Value (HT.Number 100.0)
          , Sig.in_ "height" $ HW.Value (HT.Number 100.0)
          ]
          []
        "hush" -> Just $ sig "hush" [] []
        "setFunction" -> Just $ sig "setFunction" [ Sig.inx_ "fn" ] []
        "speed" -> Just $ sig "speed" [ Sig.in_ "v" $ HW.Value (HT.Number 1.0) ] []
        "bpm" -> Just $ sig "bpm" [ Sig.in_ "v" $ HW.Value (HT.Number 30.0) ] []
        "width" -> Just $ sig "width" [] [ Sig.out_ "w" $ HW.Value HT.Width ]
        "height" -> Just $ sig "height" [] [ Sig.out_ "h" $ HW.Value HT.Height ]
        "fast" -> Just $ sig "fast"
          [ Sig.in_ "arr" $ HW.Values (HT.Values []), Sig.in_ "speed" $ HW.Value (HT.Number 1.0) ]
          [ Sig.out_ "arr" $ HW.Value (HT.VArray (HT.Values []) HT.Linear) ]
        "smooth" -> Just $ sig "smooth"
          [ Sig.in_ "arr" $ HW.Values (HT.Values []), Sig.in_ "smooth" $ HW.Value (HT.Number 1.0) ]
          [ Sig.out_ "arr" $ HW.Value (HT.VArray (HT.Values []) HT.Linear) ]
        "ease" -> Just $ sig "ease"
          [ Sig.in_ "arr" $ HW.Values (HT.Values []), Sig.in_ "ease" $ HW.Ease HT.Linear ]
          [ Sig.out_ "arr" $ HW.Value (HT.VArray (HT.Values []) HT.Linear) ]
        "offset" -> Just $ sig "offset"
          [ Sig.in_ "arr" $ HW.Values (HT.Values []), Sig.in_ "offset" $ HW.Value (HT.Number 0.5) ]
          [ Sig.out_ "arr" $ HW.Value (HT.VArray (HT.Values []) HT.Linear) ]
        "fit" -> Just $ sig "fit"
          [ Sig.in_ "arr" $ HW.Values (HT.Values [])
          , Sig.in_ "low" $ HW.Value (HT.Number 0.0)
          , Sig.in_ "high" $ HW.Value (HT.Number 1.0)
          ]
          [ Sig.out_ "arr" $ HW.Value (HT.VArray (HT.Values []) HT.Linear) ]
        "fft" -> Just $ sig "fft" [ Sig.in_ "bin" $ HW.AudioBin (HT.AudioBin 0) ]
          [ Sig.out_ "fft" $ HW.Value HT.None ]
        "setSmooth" -> Just $ sig "setSmooth"
          [ Sig.in_ "audio" $ HW.Audio HT.Silence, Sig.in_ "smooth" $ HW.Value (HT.Number 0.4) ]
          []
        "setCutoff" -> Just $ sig "setCutoff"
          [ Sig.in_ "audio" $ HW.Audio HT.Silence, Sig.in_ "cutoff" $ HW.Value (HT.Number 2.0) ]
          []
        "setBins" -> Just $ sig "setBins"
          [ Sig.in_ "audio" $ HW.Audio HT.Silence, Sig.in_ "numBins" $ HW.Value (HT.Number 4.0) ]
          []
        "setScale" -> Just $ sig "setScale"
          [ Sig.in_ "audio" $ HW.Audio HT.Silence, Sig.in_ "scale" $ HW.Value (HT.Number 10.0) ]
          []
        "hide" -> Just $ sig "hide"
          [ Sig.in_ "audio" $ HW.Audio HT.Silence, Sig.in_ "todo" $ HW.TODO HT.TODO ]
          []
        "show" -> Just $ sig "show"
          [ Sig.in_ "audio" $ HW.Audio HT.Silence, Sig.in_ "todo" $ HW.TODO HT.TODO ]
          []
        "out" -> Just $ sig "out"
          [ Sig.in_ "what" $ HW.Texture HT.Empty, Sig.in_ "target" $ HW.Unit unit ]
          []
        _ -> Nothing
    >>> map Sig.toChanneled
