module Toolkit.HydraGen where


import Prelude (Unit)

import Noodle.Toolkit3 (Toolkit) as Noodle
import Noodle.Toolkit3 as Toolkit



import Toolkit.HydraGen.Family.Source.FNoise as FNoise
import Toolkit.HydraGen.Family.Source.FVoronoi as FVoronoi
import Toolkit.HydraGen.Family.Source.FOsc as FOsc
import Toolkit.HydraGen.Family.Source.FShape as FShape
import Toolkit.HydraGen.Family.Source.FGradient as FGradient
import Toolkit.HydraGen.Family.Source.FSrc as FSrc
import Toolkit.HydraGen.Family.Source.FSolid as FSolid
import Toolkit.HydraGen.Family.Source.FPrev as FPrev
import Toolkit.HydraGen.Family.Geometry.FRotate as FRotate
import Toolkit.HydraGen.Family.Geometry.FScale as FScale
import Toolkit.HydraGen.Family.Geometry.FPixelate as FPixelate
import Toolkit.HydraGen.Family.Geometry.FRepeat as FRepeat
import Toolkit.HydraGen.Family.Geometry.FRepeatX as FRepeatX
import Toolkit.HydraGen.Family.Geometry.FRepeatY as FRepeatY
import Toolkit.HydraGen.Family.Geometry.FKaleid as FKaleid
import Toolkit.HydraGen.Family.Geometry.FScroll as FScroll
import Toolkit.HydraGen.Family.Geometry.FScrollX as FScrollX
import Toolkit.HydraGen.Family.Geometry.FScrollY as FScrollY
import Toolkit.HydraGen.Family.Color.FPosterize as FPosterize
import Toolkit.HydraGen.Family.Color.FShift as FShift
import Toolkit.HydraGen.Family.Color.FInvert as FInvert
import Toolkit.HydraGen.Family.Color.FContrast as FContrast
import Toolkit.HydraGen.Family.Color.FBrightness as FBrightness
import Toolkit.HydraGen.Family.Color.FLuma as FLuma
import Toolkit.HydraGen.Family.Color.FTresh as FTresh
import Toolkit.HydraGen.Family.Color.FColor as FColor
import Toolkit.HydraGen.Family.Color.FSaturate as FSaturate
import Toolkit.HydraGen.Family.Color.FHue as FHue
import Toolkit.HydraGen.Family.Color.FColorama as FColorama
import Toolkit.HydraGen.Family.Color.FSum as FSum
import Toolkit.HydraGen.Family.Color.FR as FR
import Toolkit.HydraGen.Family.Color.FG as FG
import Toolkit.HydraGen.Family.Color.FB as FB
import Toolkit.HydraGen.Family.Color.FA as FA
import Toolkit.HydraGen.Family.Blend.FAdd as FAdd
import Toolkit.HydraGen.Family.Blend.FSub as FSub
import Toolkit.HydraGen.Family.Blend.FLayer as FLayer
import Toolkit.HydraGen.Family.Blend.FBlend as FBlend
import Toolkit.HydraGen.Family.Blend.FMult as FMult
import Toolkit.HydraGen.Family.Blend.FDiff as FDiff
import Toolkit.HydraGen.Family.Blend.FMask as FMask
import Toolkit.HydraGen.Family.Modulate.FModulateRepeat as FModulateRepeat
import Toolkit.HydraGen.Family.Modulate.FModulateRepeatX as FModulateRepeatX
import Toolkit.HydraGen.Family.Modulate.FModulateRepeatY as FModulateRepeatY
import Toolkit.HydraGen.Family.Modulate.FModulateKaleid as FModulateKaleid
import Toolkit.HydraGen.Family.Modulate.FModulateScrollX as FModulateScrollX
import Toolkit.HydraGen.Family.Modulate.FModulateScrollY as FModulateScrollY
import Toolkit.HydraGen.Family.Modulate.FModulate as FModulate
import Toolkit.HydraGen.Family.Modulate.FModulateScale as FModulateScale
import Toolkit.HydraGen.Family.Modulate.FModulatePixelate as FModulatePixelate
import Toolkit.HydraGen.Family.Modulate.FModulateRotate as FModulateRotate
import Toolkit.HydraGen.Family.Modulate.FModulateHue as FModulateHue
import Toolkit.HydraGen.Family.Synth.FRender as FRender
import Toolkit.HydraGen.Family.Synth.FUpdate as FUpdate
import Toolkit.HydraGen.Family.Synth.FSetResolution as FSetResolution
import Toolkit.HydraGen.Family.Synth.FHush as FHush
import Toolkit.HydraGen.Family.Synth.FSetFunction as FSetFunction
import Toolkit.HydraGen.Family.Synth.FSpeed as FSpeed
import Toolkit.HydraGen.Family.Synth.FBpm as FBpm
import Toolkit.HydraGen.Family.Synth.FWidth as FWidth
import Toolkit.HydraGen.Family.Synth.FHeight as FHeight
import Toolkit.HydraGen.Family.Synth.FPi as FPi
import Toolkit.HydraGen.Family.Synth.FTime as FTime
import Toolkit.HydraGen.Family.Synth.FMouse as FMouse
import Toolkit.HydraGen.Family.Extsource.FInitCam as FInitCam
import Toolkit.HydraGen.Family.Extsource.FInitImage as FInitImage
import Toolkit.HydraGen.Family.Extsource.FInitVideo as FInitVideo
import Toolkit.HydraGen.Family.Extsource.FInit as FInit
import Toolkit.HydraGen.Family.Extsource.FInitStream as FInitStream
import Toolkit.HydraGen.Family.Extsource.FInitScreen as FInitScreen
import Toolkit.HydraGen.Family.Array.FFast as FFast
import Toolkit.HydraGen.Family.Array.FSmooth as FSmooth
import Toolkit.HydraGen.Family.Array.FEase as FEase
import Toolkit.HydraGen.Family.Array.FOffset as FOffset
import Toolkit.HydraGen.Family.Array.FFit as FFit
import Toolkit.HydraGen.Family.Audio.FFft as FFft
import Toolkit.HydraGen.Family.Audio.FSetSmooth as FSetSmooth
import Toolkit.HydraGen.Family.Audio.FSetCutoff as FSetCutoff
import Toolkit.HydraGen.Family.Audio.FSetBins as FSetBins
import Toolkit.HydraGen.Family.Audio.FSetScale as FSetScale
import Toolkit.HydraGen.Family.Audio.FHide as FHide
import Toolkit.HydraGen.Family.Audio.FShow as FShow
import Toolkit.HydraGen.Family.Out.FOut as FOut


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