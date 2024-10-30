module Hydra.Toolkit where

import Prelude ((#))
import Effect (Effect)
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Test.Files.CodeGenTest.Hydra.Color.A as Color.A
import Test.Files.CodeGenTest.Hydra.Blend.Add as Blend.Add
import Test.Files.CodeGenTest.Hydra.Feed.Array as Feed.Array
import Test.Files.CodeGenTest.Hydra.Color.B as Color.B
import Test.Files.CodeGenTest.Hydra.Blend.Blend as Blend.Blend
import Test.Files.CodeGenTest.Hydra.Synth.Bpm as Synth.Bpm
import Test.Files.CodeGenTest.Hydra.Color.Brightness as Color.Brightness
import Test.Files.CodeGenTest.Hydra.Color.Color as Color.Color
import Test.Files.CodeGenTest.Hydra.Color.Colorama as Color.Colorama
import Test.Files.CodeGenTest.Hydra.Color.Contrast as Color.Contrast
import Test.Files.CodeGenTest.Hydra.Blend.Diff as Blend.Diff
import Test.Files.CodeGenTest.Hydra.Array.Ease as Array.Ease
import Test.Files.CodeGenTest.Hydra.Feed.Expression as Feed.Expression
import Test.Files.CodeGenTest.Hydra.Array.Fast as Array.Fast
import Test.Files.CodeGenTest.Hydra.Audio.Fft as Audio.Fft
import Test.Files.CodeGenTest.Hydra.Array.Fit as Array.Fit
import Test.Files.CodeGenTest.Hydra.Color.G as Color.G
import Test.Files.CodeGenTest.Hydra.Feed.GlslFn as Feed.GlslFn
import Test.Files.CodeGenTest.Hydra.Source.Gradient as Source.Gradient
import Test.Files.CodeGenTest.Hydra.Synth.Height as Synth.Height
import Test.Files.CodeGenTest.Hydra.Audio.Hide as Audio.Hide
import Test.Files.CodeGenTest.Hydra.Color.Hue as Color.Hue
import Test.Files.CodeGenTest.Hydra.Synth.Hush as Synth.Hush
import Test.Files.CodeGenTest.Hydra.Extsource.Init as Extsource.Init
import Test.Files.CodeGenTest.Hydra.Extsource.InitCam as Extsource.InitCam
import Test.Files.CodeGenTest.Hydra.Extsource.InitImage as Extsource.InitImage
import Test.Files.CodeGenTest.Hydra.Extsource.InitScreen as Extsource.InitScreen
import Test.Files.CodeGenTest.Hydra.Extsource.InitStream as Extsource.InitStream
import Test.Files.CodeGenTest.Hydra.Extsource.InitVideo as Extsource.InitVideo
import Test.Files.CodeGenTest.Hydra.Color.Invert as Color.Invert
import Test.Files.CodeGenTest.Hydra.Geometry.Kaleid as Geometry.Kaleid
import Test.Files.CodeGenTest.Hydra.Blend.Layer as Blend.Layer
import Test.Files.CodeGenTest.Hydra.Color.Luma as Color.Luma
import Test.Files.CodeGenTest.Hydra.Blend.Mask as Blend.Mask
import Test.Files.CodeGenTest.Hydra.Modulate.Modulate as Modulate.Modulate
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateHue as Modulate.ModulateHue
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateKaleid as Modulate.ModulateKaleid
import Test.Files.CodeGenTest.Hydra.Modulate.ModulatePixelate as Modulate.ModulatePixelate
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateRepeat as Modulate.ModulateRepeat
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateRepeatX as Modulate.ModulateRepeatX
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateRepeatY as Modulate.ModulateRepeatY
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateRotate as Modulate.ModulateRotate
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateScale as Modulate.ModulateScale
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateScrollX as Modulate.ModulateScrollX
import Test.Files.CodeGenTest.Hydra.Modulate.ModulateScrollY as Modulate.ModulateScrollY
import Test.Files.CodeGenTest.Hydra.Feed.Mouse as Feed.Mouse
import Test.Files.CodeGenTest.Hydra.Blend.Mult as Blend.Mult
import Test.Files.CodeGenTest.Hydra.Source.Noise as Source.Noise
import Test.Files.CodeGenTest.Hydra.Feed.Number as Feed.Number
import Test.Files.CodeGenTest.Hydra.Array.Offset as Array.Offset
import Test.Files.CodeGenTest.Hydra.Source.Osc as Source.Osc
import Test.Files.CodeGenTest.Hydra.Out.Out as Out.Out
import Test.Files.CodeGenTest.Hydra.Feed.Pi as Feed.Pi
import Test.Files.CodeGenTest.Hydra.Geometry.Pixelate as Geometry.Pixelate
import Test.Files.CodeGenTest.Hydra.Color.Posterize as Color.Posterize
import Test.Files.CodeGenTest.Hydra.Source.Prev as Source.Prev
import Test.Files.CodeGenTest.Hydra.Color.R as Color.R
import Test.Files.CodeGenTest.Hydra.Synth.Render as Synth.Render
import Test.Files.CodeGenTest.Hydra.Geometry.Repeat as Geometry.Repeat
import Test.Files.CodeGenTest.Hydra.Geometry.RepeatX as Geometry.RepeatX
import Test.Files.CodeGenTest.Hydra.Geometry.RepeatY as Geometry.RepeatY
import Test.Files.CodeGenTest.Hydra.Geometry.Rotate as Geometry.Rotate
import Test.Files.CodeGenTest.Hydra.Color.Saturate as Color.Saturate
import Test.Files.CodeGenTest.Hydra.Geometry.Scale as Geometry.Scale
import Test.Files.CodeGenTest.Hydra.Geometry.Scroll as Geometry.Scroll
import Test.Files.CodeGenTest.Hydra.Geometry.ScrollX as Geometry.ScrollX
import Test.Files.CodeGenTest.Hydra.Geometry.ScrollY as Geometry.ScrollY
import Test.Files.CodeGenTest.Hydra.Audio.SetBins as Audio.SetBins
import Test.Files.CodeGenTest.Hydra.Audio.SetCutoff as Audio.SetCutoff
import Test.Files.CodeGenTest.Hydra.Synth.SetFunction as Synth.SetFunction
import Test.Files.CodeGenTest.Hydra.Synth.SetResolution as Synth.SetResolution
import Test.Files.CodeGenTest.Hydra.Audio.SetScale as Audio.SetScale
import Test.Files.CodeGenTest.Hydra.Audio.SetSmooth as Audio.SetSmooth
import Test.Files.CodeGenTest.Hydra.Source.Shape as Source.Shape
import Test.Files.CodeGenTest.Hydra.Color.Shift as Color.Shift
import Test.Files.CodeGenTest.Hydra.Audio.Show as Audio.Show
import Test.Files.CodeGenTest.Hydra.Array.Smooth as Array.Smooth
import Test.Files.CodeGenTest.Hydra.Source.Solid as Source.Solid
import Test.Files.CodeGenTest.Hydra.Synth.Speed as Synth.Speed
import Test.Files.CodeGenTest.Hydra.Source.Src as Source.Src
import Test.Files.CodeGenTest.Hydra.Blend.Sub as Blend.Sub
import Test.Files.CodeGenTest.Hydra.Color.Sum as Color.Sum
import Test.Files.CodeGenTest.Hydra.Color.Thresh as Color.Thresh
import Test.Files.CodeGenTest.Hydra.Feed.Time as Feed.Time
import Test.Files.CodeGenTest.Hydra.Synth.Update as Synth.Update
import Test.Files.CodeGenTest.Hydra.Source.Voronoi as Source.Voronoi
import Test.Files.CodeGenTest.Hydra.Synth.Width as Synth.Width
import Hydra.Repr.Wrap (WrapRepr)

type HydraFamilies :: Families
type HydraFamilies = Color.A.F :> Blend.Add.F :> Feed.Array.F :> Color.B.F :> Blend.Blend.F
  :> Synth.Bpm.F
  :> Color.Brightness.F
  :> Color.Color.F
  :> Color.Colorama.F
  :> Color.Contrast.F
  :> Blend.Diff.F
  :> Array.Ease.F
  :> Feed.Expression.F
  :> Array.Fast.F
  :> Audio.Fft.F
  :> Array.Fit.F
  :> Color.G.F
  :> Feed.GlslFn.F
  :> Source.Gradient.F
  :> Synth.Height.F
  :> Audio.Hide.F
  :> Color.Hue.F
  :> Synth.Hush.F
  :> Extsource.Init.F
  :> Extsource.InitCam.F
  :> Extsource.InitImage.F
  :> Extsource.InitScreen.F
  :> Extsource.InitStream.F
  :> Extsource.InitVideo.F
  :> Color.Invert.F
  :> Geometry.Kaleid.F
  :> Blend.Layer.F
  :> Color.Luma.F
  :> Blend.Mask.F
  :> Modulate.Modulate.F
  :> Modulate.ModulateHue.F
  :> Modulate.ModulateKaleid.F
  :> Modulate.ModulatePixelate.F
  :> Modulate.ModulateRepeat.F
  :> Modulate.ModulateRepeatX.F
  :> Modulate.ModulateRepeatY.F
  :> Modulate.ModulateRotate.F
  :> Modulate.ModulateScale.F
  :> Modulate.ModulateScrollX.F
  :> Modulate.ModulateScrollY.F
  :> Feed.Mouse.F
  :> Blend.Mult.F
  :> Source.Noise.F
  :> Feed.Number.F
  :> Array.Offset.F
  :> Source.Osc.F
  :> Out.Out.F
  :> Feed.Pi.F
  :> Geometry.Pixelate.F
  :> Color.Posterize.F
  :> Source.Prev.F
  :> Color.R.F
  :> Synth.Render.F
  :> Geometry.Repeat.F
  :> Geometry.RepeatX.F
  :> Geometry.RepeatY.F
  :> Geometry.Rotate.F
  :> Color.Saturate.F
  :> Geometry.Scale.F
  :> Geometry.Scroll.F
  :> Geometry.ScrollX.F
  :> Geometry.ScrollY.F
  :> Audio.SetBins.F
  :> Audio.SetCutoff.F
  :> Synth.SetFunction.F
  :> Synth.SetResolution.F
  :> Audio.SetScale.F
  :> Audio.SetSmooth.F
  :> Source.Shape.F
  :> Color.Shift.F
  :> Audio.Show.F
  :> Array.Smooth.F
  :> Source.Solid.F
  :> Synth.Speed.F
  :> Source.Src.F
  :> Blend.Sub.F
  :> Color.Sum.F
  :> Color.Thresh.F
  :> Feed.Time.F
  :> Synth.Update.F
  :> Source.Voronoi.F
  :> Synth.Width.F
  :> TNil

toolkit :: Toolkit HydraFamilies WrapRepr Effect
toolkit = Toolkit.empty "Hydra" # Toolkit.register Synth.Width.family
  # Toolkit.register Source.Voronoi.family
  # Toolkit.register Synth.Update.family
  # Toolkit.register Feed.Time.family
  # Toolkit.register Color.Thresh.family
  # Toolkit.register Color.Sum.family
  # Toolkit.register Blend.Sub.family
  # Toolkit.register Source.Src.family
  # Toolkit.register Synth.Speed.family
  # Toolkit.register Source.Solid.family
  # Toolkit.register Array.Smooth.family
  # Toolkit.register Audio.Show.family
  # Toolkit.register Color.Shift.family
  # Toolkit.register Source.Shape.family
  # Toolkit.register Audio.SetSmooth.family
  # Toolkit.register Audio.SetScale.family
  # Toolkit.register Synth.SetResolution.family
  # Toolkit.register Synth.SetFunction.family
  # Toolkit.register Audio.SetCutoff.family
  # Toolkit.register Audio.SetBins.family
  # Toolkit.register Geometry.ScrollY.family
  # Toolkit.register Geometry.ScrollX.family
  # Toolkit.register Geometry.Scroll.family
  # Toolkit.register Geometry.Scale.family
  # Toolkit.register Color.Saturate.family
  # Toolkit.register Geometry.Rotate.family
  # Toolkit.register Geometry.RepeatY.family
  # Toolkit.register Geometry.RepeatX.family
  # Toolkit.register Geometry.Repeat.family
  # Toolkit.register Synth.Render.family
  # Toolkit.register Color.R.family
  # Toolkit.register Source.Prev.family
  # Toolkit.register Color.Posterize.family
  # Toolkit.register Geometry.Pixelate.family
  # Toolkit.register Feed.Pi.family
  # Toolkit.register Out.Out.family
  # Toolkit.register Source.Osc.family
  # Toolkit.register Array.Offset.family
  # Toolkit.register Feed.Number.family
  # Toolkit.register Source.Noise.family
  # Toolkit.register Blend.Mult.family
  # Toolkit.register Feed.Mouse.family
  # Toolkit.register Modulate.ModulateScrollY.family
  # Toolkit.register Modulate.ModulateScrollX.family
  # Toolkit.register Modulate.ModulateScale.family
  # Toolkit.register Modulate.ModulateRotate.family
  # Toolkit.register Modulate.ModulateRepeatY.family
  # Toolkit.register Modulate.ModulateRepeatX.family
  # Toolkit.register Modulate.ModulateRepeat.family
  # Toolkit.register Modulate.ModulatePixelate.family
  # Toolkit.register Modulate.ModulateKaleid.family
  # Toolkit.register Modulate.ModulateHue.family
  # Toolkit.register Modulate.Modulate.family
  # Toolkit.register Blend.Mask.family
  # Toolkit.register Color.Luma.family
  # Toolkit.register Blend.Layer.family
  # Toolkit.register Geometry.Kaleid.family
  # Toolkit.register Color.Invert.family
  # Toolkit.register Extsource.InitVideo.family
  # Toolkit.register Extsource.InitStream.family
  # Toolkit.register Extsource.InitScreen.family
  # Toolkit.register Extsource.InitImage.family
  # Toolkit.register Extsource.InitCam.family
  # Toolkit.register Extsource.Init.family
  # Toolkit.register Synth.Hush.family
  # Toolkit.register Color.Hue.family
  # Toolkit.register Audio.Hide.family
  # Toolkit.register Synth.Height.family
  # Toolkit.register Source.Gradient.family
  # Toolkit.register Feed.GlslFn.family
  # Toolkit.register Color.G.family
  # Toolkit.register Array.Fit.family
  # Toolkit.register Audio.Fft.family
  # Toolkit.register Array.Fast.family
  # Toolkit.register Feed.Expression.family
  # Toolkit.register Array.Ease.family
  # Toolkit.register Blend.Diff.family
  # Toolkit.register Color.Contrast.family
  # Toolkit.register Color.Colorama.family
  # Toolkit.register Color.Color.family
  # Toolkit.register Color.Brightness.family
  # Toolkit.register Synth.Bpm.family
  # Toolkit.register Blend.Blend.family
  # Toolkit.register Color.B.family
  # Toolkit.register Feed.Array.family
  # Toolkit.register Blend.Add.family
  # Toolkit.register Color.A.family
