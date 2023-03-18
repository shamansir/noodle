module Toolkit.Hydra2 (HydraToolkit, toolkit, Toolkit, Instances) where


import Prelude (Unit)

import Noodle.Toolkit3 (Toolkit) as Noodle
import Noodle.Toolkit3 as Toolkit


import Toolkit.Hydra2.Family.Source.FNoise as FNoise
import Toolkit.Hydra2.Family.Source.FVoronoi as FVoronoi
import Toolkit.Hydra2.Family.Source.FOsc as FOsc
import Toolkit.Hydra2.Family.Source.FShape as FShape
import Toolkit.Hydra2.Family.Source.FGradient as FGradient
import Toolkit.Hydra2.Family.Source.FSrc as FSrc
import Toolkit.Hydra2.Family.Source.FSolid as FSolid
import Toolkit.Hydra2.Family.Source.FPrev as FPrev
import Toolkit.Hydra2.Family.Geometry.FRotate as FRotate
import Toolkit.Hydra2.Family.Geometry.FScale as FScale
import Toolkit.Hydra2.Family.Geometry.FPixelate as FPixelate
import Toolkit.Hydra2.Family.Geometry.FRepeat as FRepeat
import Toolkit.Hydra2.Family.Geometry.FRepeatX as FRepeatX
import Toolkit.Hydra2.Family.Geometry.FRepeatY as FRepeatY
import Toolkit.Hydra2.Family.Geometry.FKaleid as FKaleid
import Toolkit.Hydra2.Family.Geometry.FScroll as FScroll
import Toolkit.Hydra2.Family.Geometry.FScrollX as FScrollX
import Toolkit.Hydra2.Family.Geometry.FScrollY as FScrollY
import Toolkit.Hydra2.Family.Color.FPosterize as FPosterize
import Toolkit.Hydra2.Family.Color.FShift as FShift
import Toolkit.Hydra2.Family.Color.FInvert as FInvert
import Toolkit.Hydra2.Family.Color.FContrast as FContrast
import Toolkit.Hydra2.Family.Color.FBrightness as FBrightness
import Toolkit.Hydra2.Family.Color.FLuma as FLuma
import Toolkit.Hydra2.Family.Color.FTresh as FTresh
import Toolkit.Hydra2.Family.Color.FColor as FColor
import Toolkit.Hydra2.Family.Color.FSaturate as FSaturate
import Toolkit.Hydra2.Family.Color.FHue as FHue
import Toolkit.Hydra2.Family.Color.FColorama as FColorama
import Toolkit.Hydra2.Family.Color.FSum as FSum
import Toolkit.Hydra2.Family.Color.FR as FR
import Toolkit.Hydra2.Family.Color.FG as FG
import Toolkit.Hydra2.Family.Color.FB as FB
import Toolkit.Hydra2.Family.Color.FA as FA
import Toolkit.Hydra2.Family.Blend.FAdd as FAdd
import Toolkit.Hydra2.Family.Blend.FSub as FSub
import Toolkit.Hydra2.Family.Blend.FLayer as FLayer
import Toolkit.Hydra2.Family.Blend.FBlend as FBlend
import Toolkit.Hydra2.Family.Blend.FMult as FMult
import Toolkit.Hydra2.Family.Blend.FDiff as FDiff
import Toolkit.Hydra2.Family.Blend.FMask as FMask
import Toolkit.Hydra2.Family.Modulate.FModulateRepeat as FModulateRepeat
import Toolkit.Hydra2.Family.Modulate.FModulateRepeatX as FModulateRepeatX
import Toolkit.Hydra2.Family.Modulate.FModulateRepeatY as FModulateRepeatY
import Toolkit.Hydra2.Family.Modulate.FModulateKaleid as FModulateKaleid
import Toolkit.Hydra2.Family.Modulate.FModulateScrollX as FModulateScrollX
import Toolkit.Hydra2.Family.Modulate.FModulateScrollY as FModulateScrollY
import Toolkit.Hydra2.Family.Modulate.FModulate as FModulate
import Toolkit.Hydra2.Family.Modulate.FModulateScale as FModulateScale
import Toolkit.Hydra2.Family.Modulate.FModulatePixelate as FModulatePixelate
import Toolkit.Hydra2.Family.Modulate.FModulateRotate as FModulateRotate
import Toolkit.Hydra2.Family.Modulate.FModulateHue as FModulateHue
import Toolkit.Hydra2.Family.Synth.FRender as FRender
import Toolkit.Hydra2.Family.Synth.FUpdate as FUpdate
import Toolkit.Hydra2.Family.Synth.FSetResolution as FSetResolution
import Toolkit.Hydra2.Family.Synth.FHush as FHush
import Toolkit.Hydra2.Family.Synth.FSetFunction as FSetFunction
import Toolkit.Hydra2.Family.Synth.FSpeed as FSpeed
import Toolkit.Hydra2.Family.Synth.FBpm as FBpm
import Toolkit.Hydra2.Family.Synth.FWidth as FWidth
import Toolkit.Hydra2.Family.Synth.FHeight as FHeight
import Toolkit.Hydra2.Family.Synth.FPi as FPi
import Toolkit.Hydra2.Family.Synth.FTime as FTime
import Toolkit.Hydra2.Family.Synth.FMouse as FMouse
import Toolkit.Hydra2.Family.Extsource.FInitCam as FInitCam
import Toolkit.Hydra2.Family.Extsource.FInitImage as FInitImage
import Toolkit.Hydra2.Family.Extsource.FInitVideo as FInitVideo
import Toolkit.Hydra2.Family.Extsource.FInit as FInit
import Toolkit.Hydra2.Family.Extsource.FInitStream as FInitStream
import Toolkit.Hydra2.Family.Extsource.FInitScreen as FInitScreen
import Toolkit.Hydra2.Family.Array.FFast as FFast
import Toolkit.Hydra2.Family.Array.FSmooth as FSmooth
import Toolkit.Hydra2.Family.Array.FEase as FEase
import Toolkit.Hydra2.Family.Array.FOffset as FOffset
import Toolkit.Hydra2.Family.Array.FFit as FFit
import Toolkit.Hydra2.Family.Audio.FFft as FFft
import Toolkit.Hydra2.Family.Audio.FSetSmooth as FSetSmooth
import Toolkit.Hydra2.Family.Audio.FSetCutoff as FSetCutoff
import Toolkit.Hydra2.Family.Audio.FSetBins as FSetBins
import Toolkit.Hydra2.Family.Audio.FSetScale as FSetScale
import Toolkit.Hydra2.Family.Audio.FHide as FHide
import Toolkit.Hydra2.Family.Audio.FShow as FShow
import Toolkit.Hydra2.Family.Out.FOut as FOut


type HydraToolkit m
    = Noodle.Toolkit Unit
        ( noise :: FNoise.Family m  -- {-> source <-}
        , voronoi :: FVoronoi.Family m  -- {-> source <-}
        , osc :: FOsc.Family m -- {-> source <-}
        , shape :: FShape.Family m  -- {-> source <-}
        , gradient :: FGradient.Family m  -- {-> source <-}
        , src :: FSrc.Family m  -- {-> source <-}
        , solid :: FSolid.Family m  -- {-> source <-}
        , prev :: FPrev.Family m  -- {-> source <-}
        , rotate :: FRotate.Family m  -- {-> geometry <-}
        , scale :: FScale.Family m  -- {-> geometry <-}
        , pixelate :: FPixelate.Family m  -- {-> geometry <-}
        , repeat :: FRepeat.Family m  -- {-> geometry <-}
        , repeatX :: FRepeatX.Family m  -- {-> geometry <-}
        , repeatY :: FRepeatY.Family m  -- {-> geometry <-}
        , kaleid :: FKaleid.Family m  -- {-> geometry <-}
        , scroll :: FScroll.Family m  -- {-> geometry <-}
        , scrollX :: FScrollX.Family m  -- {-> geometry <-}
        , scrollY :: FScrollY.Family m  -- {-> geometry <-}
        , posterize :: FPosterize.Family m  -- {-> color <-}
        , shift :: FShift.Family m  -- {-> color <-}
        , invert :: FInvert.Family m  -- {-> color <-}
        , contrast :: FContrast.Family m  -- {-> color <-}
        , brightness :: FBrightness.Family m  -- {-> color <-}
        , luma :: FLuma.Family m  -- {-> color <-}
        , tresh :: FTresh.Family m  -- {-> color <-}
        , color :: FColor.Family m  -- {-> color <-}
        , saturate :: FSaturate.Family m  -- {-> color <-}
        , hue :: FHue.Family m  -- {-> color <-}
        , colorama :: FColorama.Family m  -- {-> color <-}
        , sum :: FSum.Family m  -- {-> color <-}
        , r :: FR.Family m  -- {-> color <-}
        , g :: FG.Family m  -- {-> color <-}
        , b :: FB.Family m  -- {-> color <-}
        , a :: FA.Family m  -- {-> color <-}
        , add :: FAdd.Family m  -- {-> blend <-}
        , sub :: FSub.Family m  -- {-> blend <-}
        , layer :: FLayer.Family m  -- {-> blend <-}
        , blend :: FBlend.Family m  -- {-> blend <-}
        , mult :: FMult.Family m  -- {-> blend <-}
        , diff :: FDiff.Family m  -- {-> blend <-}
        , mask :: FMask.Family m  -- {-> blend <-}
        , modulateRepeat :: FModulateRepeat.Family m  -- {-> modulate <-}
        , modulateRepeatX :: FModulateRepeatX.Family m  -- {-> modulate <-}
        , modulateRepeatY :: FModulateRepeatY.Family m  -- {-> modulate <-}
        , modulateKaleid :: FModulateKaleid.Family m  -- {-> modulate <-}
        , modulateScrollX :: FModulateScrollX.Family m  -- {-> modulate <-}
        , modulateScrollY :: FModulateScrollY.Family m  -- {-> modulate <-}
        , modulate :: FModulate.Family m  -- {-> modulate <-}
        , modulateScale :: FModulateScale.Family m  -- {-> modulate <-}
        , modulatePixelate :: FModulatePixelate.Family m  -- {-> modulate <-}
        , modulateRotate :: FModulateRotate.Family m  -- {-> modulate <-}
        , modulateHue :: FModulateHue.Family m  -- {-> modulate <-}
        , render :: FRender.Family m  -- {-> synth <-}
        , update :: FUpdate.Family m  -- {-> synth <-}
        , setResolution :: FSetResolution.Family m  -- {-> synth <-}
        , hush :: FHush.Family m  -- {-> synth <-}
        , setFunction :: FSetFunction.Family m  -- {-> synth <-}
        , speed :: FSpeed.Family m  -- {-> synth <-}
        , bpm :: FBpm.Family m  -- {-> synth <-}
        , width :: FWidth.Family m  -- {-> synth <-}
        , height :: FHeight.Family m  -- {-> synth <-}
        , pi :: FPi.Family m  -- {-> synth <-}
        , time :: FTime.Family m  -- {-> synth <-}
        , mouse :: FMouse.Family m  -- {-> synth <-}
        , initCam :: FInitCam.Family m  -- {-> extsource <-}
        , initImage :: FInitImage.Family m  -- {-> extsource <-}
        , initVideo :: FInitVideo.Family m  -- {-> extsource <-}
        , init :: FInit.Family m  -- {-> extsource <-}
        , initStream :: FInitStream.Family m  -- {-> extsource <-}
        , initScreen :: FInitScreen.Family m  -- {-> extsource <-}
        , fast :: FFast.Family m  -- {-> array <-}
        , smooth :: FSmooth.Family m  -- {-> array <-}
        , ease :: FEase.Family m  -- {-> array <-}
        , offset :: FOffset.Family m  -- {-> array <-}
        , fit :: FFit.Family m  -- {-> array <-}
        , fft :: FFft.Family m  -- {-> audio <-}
        , setSmooth :: FSetSmooth.Family m  -- {-> audio <-}
        , setCutoff :: FSetCutoff.Family m  -- {-> audio <-}
        , setBins :: FSetBins.Family m  -- {-> audio <-}
        , setScale :: FSetScale.Family m  -- {-> audio <-}
        , hide :: FHide.Family m  -- {-> audio <-}
        , show :: FShow.Family m  -- {-> audio <-}
        , out :: FOut.Family m  -- {-> out <-}
        )


toolkit :: forall m. HydraToolkit m
toolkit =
    Toolkit.from "hydra"
        { noise : (FNoise.family :: FNoise.Family m)
        , voronoi : (FVoronoi.family :: FVoronoi.Family m)
        , osc : (FOsc.family :: FOsc.Family m)
        , shape : (FShape.family :: FShape.Family m)
        , gradient : (FGradient.family :: FGradient.Family m)
        , src : (FSrc.family :: FSrc.Family m)
        , solid : (FSolid.family :: FSolid.Family m)
        , prev : (FPrev.family :: FPrev.Family m)
        , rotate : (FRotate.family :: FRotate.Family m)
        , scale : (FScale.family :: FScale.Family m)
        , pixelate : (FPixelate.family :: FPixelate.Family m)
        , repeat : (FRepeat.family :: FRepeat.Family m)
        , repeatX : (FRepeatX.family :: FRepeatX.Family m)
        , repeatY : (FRepeatY.family :: FRepeatY.Family m)
        , kaleid : (FKaleid.family :: FKaleid.Family m)
        , scroll : (FScroll.family :: FScroll.Family m)
        , scrollX : (FScrollX.family :: FScrollX.Family m)
        , scrollY : (FScrollY.family :: FScrollY.Family m)
        , posterize : (FPosterize.family :: FPosterize.Family m)
        , shift : (FShift.family :: FShift.Family m)
        , invert : (FInvert.family :: FInvert.Family m)
        , contrast : (FContrast.family :: FContrast.Family m)
        , brightness : (FBrightness.family :: FBrightness.Family m)
        , luma : (FLuma.family :: FLuma.Family m)
        , tresh : (FTresh.family :: FTresh.Family m)
        , color : (FColor.family :: FColor.Family m)
        , saturate : (FSaturate.family :: FSaturate.Family m)
        , hue : (FHue.family :: FHue.Family m)
        , colorama : (FColorama.family :: FColorama.Family m)
        , sum : (FSum.family :: FSum.Family m)
        , r : (FR.family :: FR.Family m)
        , g : (FG.family :: FG.Family m)
        , b : (FB.family :: FB.Family m)
        , a : (FA.family :: FA.Family m)
        , add : (FAdd.family :: FAdd.Family m)
        , sub : (FSub.family :: FSub.Family m)
        , layer : (FLayer.family :: FLayer.Family m)
        , blend : (FBlend.family :: FBlend.Family m)
        , mult : (FMult.family :: FMult.Family m)
        , diff : (FDiff.family :: FDiff.Family m)
        , mask : (FMask.family :: FMask.Family m)
        , modulateRepeat : (FModulateRepeat.family :: FModulateRepeat.Family m)
        , modulateRepeatX : (FModulateRepeatX.family :: FModulateRepeatX.Family m)
        , modulateRepeatY : (FModulateRepeatY.family :: FModulateRepeatY.Family m)
        , modulateKaleid : (FModulateKaleid.family :: FModulateKaleid.Family m)
        , modulateScrollX : (FModulateScrollX.family :: FModulateScrollX.Family m)
        , modulateScrollY : (FModulateScrollY.family :: FModulateScrollY.Family m)
        , modulate : (FModulate.family :: FModulate.Family m)
        , modulateScale : (FModulateScale.family :: FModulateScale.Family m)
        , modulatePixelate : (FModulatePixelate.family :: FModulatePixelate.Family m)
        , modulateRotate : (FModulateRotate.family :: FModulateRotate.Family m)
        , modulateHue : (FModulateHue.family :: FModulateHue.Family m)
        , render : (FRender.family :: FRender.Family m)
        , update : (FUpdate.family :: FUpdate.Family m)
        , setResolution : (FSetResolution.family :: FSetResolution.Family m)
        , hush : (FHush.family :: FHush.Family m)
        , setFunction : (FSetFunction.family :: FSetFunction.Family m)
        , speed : (FSpeed.family :: FSpeed.Family m)
        , bpm : (FBpm.family :: FBpm.Family m)
        , width : (FWidth.family :: FWidth.Family m)
        , height : (FHeight.family :: FHeight.Family m)
        , pi : (FPi.family :: FPi.Family m)
        , time : (FTime.family :: FTime.Family m)
        , mouse : (FMouse.family :: FMouse.Family m)
        , initCam : (FInitCam.family :: FInitCam.Family m)
        , initImage : (FInitImage.family :: FInitImage.Family m)
        , initVideo : (FInitVideo.family :: FInitVideo.Family m)
        , init : (FInit.family :: FInit.Family m)
        , initStream : (FInitStream.family :: FInitStream.Family m)
        , initScreen : (FInitScreen.family :: FInitScreen.Family m)
        , fast : (FFast.family :: FFast.Family m)
        , smooth : (FSmooth.family :: FSmooth.Family m)
        , ease : (FEase.family :: FEase.Family m)
        , offset : (FOffset.family :: FOffset.Family m)
        , fit : (FFit.family :: FFit.Family m)
        , fft : (FFft.family :: FFft.Family m)
        , setSmooth : (FSetSmooth.family :: FSetSmooth.Family m)
        , setCutoff : (FSetCutoff.family :: FSetCutoff.Family m)
        , setBins : (FSetBins.family :: FSetBins.Family m)
        , setScale : (FSetScale.family :: FSetScale.Family m)
        , hide : (FHide.family :: FHide.Family m)
        , show : (FShow.family :: FShow.Family m)
        , out : (FOut.family :: FOut.Family m)
        }


type Toolkit m = HydraToolkit m


type Instances m =
        ( noise :: Array ( FNoise.Node m )
        , voronoi :: Array ( FVoronoi.Node m )
        , osc :: Array ( FOsc.Node m )
        , shape :: Array ( FShape.Node m )
        , gradient :: Array ( FGradient.Node m )
        , src :: Array ( FSrc.Node m )
        , solid :: Array ( FSolid.Node m )
        , src :: Array ( FSrc.Node m )
        , prev :: Array ( FPrev.Node m )
        , rotate :: Array ( FRotate.Node m )
        , scale :: Array ( FScale.Node m )
        , pixelate :: Array ( FPixelate.Node m )
        , repeat :: Array ( FRepeat.Node m )
        , repeatX :: Array ( FRepeatX.Node m )
        , repeatY :: Array ( FRepeatY.Node m )
        , kaleid :: Array ( FKaleid.Node m )
        , scroll :: Array ( FScroll.Node m )
        , scrollX :: Array ( FScrollX.Node m )
        , scrollY :: Array ( FScrollY.Node m )
        , posterize :: Array ( FPosterize.Node m )
        , shift :: Array ( FShift.Node m )
        , invert :: Array ( FInvert.Node m )
        , contrast :: Array ( FContrast.Node m )
        , brightness :: Array ( FBrightness.Node m )
        , luma :: Array ( FLuma.Node m )
        , tresh :: Array ( FTresh.Node m )
        , color :: Array ( FColor.Node m )
        , saturate :: Array ( FSaturate.Node m )
        , hue :: Array ( FHue.Node m )
        , colorama :: Array ( FColorama.Node m )
        , sum :: Array ( FSum.Node m )
        , r :: Array ( FR.Node m )
        , g :: Array ( FG.Node m )
        , b :: Array ( FB.Node m )
        , a :: Array ( FA.Node m )
        , add :: Array ( FAdd.Node m )
        , sub :: Array ( FSub.Node m )
        , layer :: Array ( FLayer.Node m )
        , blend :: Array ( FBlend.Node m )
        , mult :: Array ( FMult.Node m )
        , diff :: Array ( FDiff.Node m )
        , mask :: Array ( FMask.Node m )
        , modulateRepeat :: Array ( FModulateRepeat.Node m )
        , modulateRepeatX :: Array ( FModulateRepeatX.Node m )
        , modulateRepeatY :: Array ( FModulateRepeatY.Node m )
        , modulateKaleid :: Array ( FModulateKaleid.Node m )
        , modulateScrollX :: Array ( FModulateScrollX.Node m )
        , modulateScrollY :: Array ( FModulateScrollY.Node m )
        , modulate :: Array ( FModulate.Node m )
        , modulateScale :: Array ( FModulateScale.Node m )
        , modulatePixelate :: Array ( FModulatePixelate.Node m )
        , modulateRotate :: Array ( FModulateRotate.Node m )
        , modulateHue :: Array ( FModulateHue.Node m )
        , render :: Array ( FRender.Node m )
        , update :: Array ( FUpdate.Node m )
        , setResolution :: Array ( FSetResolution.Node m )
        , hush :: Array ( FHush.Node m )
        , setFunction :: Array ( FSetFunction.Node m )
        , speed :: Array ( FSpeed.Node m )
        , bpm :: Array ( FBpm.Node m )
        , width :: Array ( FWidth.Node m )
        , height :: Array ( FHeight.Node m )
        , pi :: Array ( FPi.Node m )
        , time :: Array ( FTime.Node m )
        , mouse :: Array ( FMouse.Node m )
        , initCam :: Array ( FInitCam.Node m )
        , initImage :: Array ( FInitImage.Node m )
        , initVideo :: Array ( FInitVideo.Node m )
        , init :: Array ( FInit.Node m )
        , initStream :: Array ( FInitStream.Node m )
        , initScreen :: Array ( FInitScreen.Node m )
        , fast :: Array ( FFast.Node m )
        , smooth :: Array ( FSmooth.Node m )
        , ease :: Array ( FEase.Node m )
        , offset :: Array ( FOffset.Node m )
        , fit :: Array ( FFit.Node m )
        , fft :: Array ( FFft.Node m )
        , setSmooth :: Array ( FSetSmooth.Node m )
        , setCutoff :: Array ( FSetCutoff.Node m )
        , setBins :: Array ( FSetBins.Node m )
        , setScale :: Array ( FSetScale.Node m )
        , hide :: Array ( FHide.Node m )
        , show :: Array ( FShow.Node m )
        , out :: Array ( FOut.Node m )
        )