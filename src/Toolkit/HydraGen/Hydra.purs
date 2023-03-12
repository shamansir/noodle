module Toolkit.HydraGen where








import Toolkit.HydraGen.Family.Source.FNoise as FNoise
import Toolkit.HydraGen.Family.Source.FVoronoi as FVoronoi
import Toolkit.HydraGen.Family.Source.FOsc as FOsc
import Toolkit.HydraGen.Family.Source.FShape as FShape
import Toolkit.HydraGen.Family.Source.FGradient as FGradient
import Toolkit.HydraGen.Family.Source.FSrc as FSrc
import Toolkit.HydraGen.Family.Source.FSolid as FSolid
import Toolkit.HydraGen.Family.Source.FSrc as FSrc
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
        ( noise :: FNoise.Family -- {-> source <-}
        , voronoi :: FVoronoi.Family -- {-> source <-}
        , osc :: FOsc.Family -- {-> source <-}
        , shape :: FShape.Family -- {-> source <-}
        , gradient :: FGradient.Family -- {-> source <-}
        , src :: FSrc.Family -- {-> source <-}
        , solid :: FSolid.Family -- {-> source <-}
        , src :: FSrc.Family -- {-> source <-}
        , prev :: FPrev.Family -- {-> source <-}
        , rotate :: FRotate.Family -- {-> geometry <-}
        , scale :: FScale.Family -- {-> geometry <-}
        , pixelate :: FPixelate.Family -- {-> geometry <-}
        , repeat :: FRepeat.Family -- {-> geometry <-}
        , repeatX :: FRepeatX.Family -- {-> geometry <-}
        , repeatY :: FRepeatY.Family -- {-> geometry <-}
        , kaleid :: FKaleid.Family -- {-> geometry <-}
        , scroll :: FScroll.Family -- {-> geometry <-}
        , scrollX :: FScrollX.Family -- {-> geometry <-}
        , scrollY :: FScrollY.Family -- {-> geometry <-}
        , posterize :: FPosterize.Family -- {-> color <-}
        , shift :: FShift.Family -- {-> color <-}
        , invert :: FInvert.Family -- {-> color <-}
        , contrast :: FContrast.Family -- {-> color <-}
        , brightness :: FBrightness.Family -- {-> color <-}
        , luma :: FLuma.Family -- {-> color <-}
        , tresh :: FTresh.Family -- {-> color <-}
        , color :: FColor.Family -- {-> color <-}
        , saturate :: FSaturate.Family -- {-> color <-}
        , hue :: FHue.Family -- {-> color <-}
        , colorama :: FColorama.Family -- {-> color <-}
        , sum :: FSum.Family -- {-> color <-}
        , r :: FR.Family -- {-> color <-}
        , g :: FG.Family -- {-> color <-}
        , b :: FB.Family -- {-> color <-}
        , a :: FA.Family -- {-> color <-}
        , add :: FAdd.Family -- {-> blend <-}
        , sub :: FSub.Family -- {-> blend <-}
        , layer :: FLayer.Family -- {-> blend <-}
        , blend :: FBlend.Family -- {-> blend <-}
        , mult :: FMult.Family -- {-> blend <-}
        , diff :: FDiff.Family -- {-> blend <-}
        , mask :: FMask.Family -- {-> blend <-}
        , modulateRepeat :: FModulateRepeat.Family -- {-> modulate <-}
        , modulateRepeatX :: FModulateRepeatX.Family -- {-> modulate <-}
        , modulateRepeatY :: FModulateRepeatY.Family -- {-> modulate <-}
        , modulateKaleid :: FModulateKaleid.Family -- {-> modulate <-}
        , modulateScrollX :: FModulateScrollX.Family -- {-> modulate <-}
        , modulateScrollY :: FModulateScrollY.Family -- {-> modulate <-}
        , modulate :: FModulate.Family -- {-> modulate <-}
        , modulateScale :: FModulateScale.Family -- {-> modulate <-}
        , modulatePixelate :: FModulatePixelate.Family -- {-> modulate <-}
        , modulateRotate :: FModulateRotate.Family -- {-> modulate <-}
        , modulateHue :: FModulateHue.Family -- {-> modulate <-}
        , render :: FRender.Family -- {-> synth <-}
        , update :: FUpdate.Family -- {-> synth <-}
        , setResolution :: FSetResolution.Family -- {-> synth <-}
        , hush :: FHush.Family -- {-> synth <-}
        , setFunction :: FSetFunction.Family -- {-> synth <-}
        , speed :: FSpeed.Family -- {-> synth <-}
        , bpm :: FBpm.Family -- {-> synth <-}
        , width :: FWidth.Family -- {-> synth <-}
        , height :: FHeight.Family -- {-> synth <-}
        , pi :: FPi.Family -- {-> synth <-}
        , time :: FTime.Family -- {-> synth <-}
        , mouse :: FMouse.Family -- {-> synth <-}
        , initCam :: FInitCam.Family -- {-> extsource <-}
        , initImage :: FInitImage.Family -- {-> extsource <-}
        , initVideo :: FInitVideo.Family -- {-> extsource <-}
        , init :: FInit.Family -- {-> extsource <-}
        , initStream :: FInitStream.Family -- {-> extsource <-}
        , initScreen :: FInitScreen.Family -- {-> extsource <-}
        , fast :: FFast.Family -- {-> array <-}
        , smooth :: FSmooth.Family -- {-> array <-}
        , ease :: FEase.Family -- {-> array <-}
        , offset :: FOffset.Family -- {-> array <-}
        , fit :: FFit.Family -- {-> array <-}
        , fft :: FFft.Family -- {-> audio <-}
        , setSmooth :: FSetSmooth.Family -- {-> audio <-}
        , setCutoff :: FSetCutoff.Family -- {-> audio <-}
        , setBins :: FSetBins.Family -- {-> audio <-}
        , setScale :: FSetScale.Family -- {-> audio <-}
        , hide :: FHide.Family -- {-> audio <-}
        , show :: FShow.Family -- {-> audio <-}
        , out :: FOut.Family -- {-> out <-}
        )


toolkit :: forall m. HydraToolkit m
toolkit =
    Toolkit.from "hydra"
        { noise : FNoise.family
        , voronoi : FVoronoi.family
        , osc : FOsc.family
        , shape : FShape.family
        , gradient : FGradient.family
        , src : FSrc.family
        , solid : FSolid.family
        , src : FSrc.family
        , prev : FPrev.family
        , rotate : FRotate.family
        , scale : FScale.family
        , pixelate : FPixelate.family
        , repeat : FRepeat.family
        , repeatX : FRepeatX.family
        , repeatY : FRepeatY.family
        , kaleid : FKaleid.family
        , scroll : FScroll.family
        , scrollX : FScrollX.family
        , scrollY : FScrollY.family
        , posterize : FPosterize.family
        , shift : FShift.family
        , invert : FInvert.family
        , contrast : FContrast.family
        , brightness : FBrightness.family
        , luma : FLuma.family
        , tresh : FTresh.family
        , color : FColor.family
        , saturate : FSaturate.family
        , hue : FHue.family
        , colorama : FColorama.family
        , sum : FSum.family
        , r : FR.family
        , g : FG.family
        , b : FB.family
        , a : FA.family
        , add : FAdd.family
        , sub : FSub.family
        , layer : FLayer.family
        , blend : FBlend.family
        , mult : FMult.family
        , diff : FDiff.family
        , mask : FMask.family
        , modulateRepeat : FModulateRepeat.family
        , modulateRepeatX : FModulateRepeatX.family
        , modulateRepeatY : FModulateRepeatY.family
        , modulateKaleid : FModulateKaleid.family
        , modulateScrollX : FModulateScrollX.family
        , modulateScrollY : FModulateScrollY.family
        , modulate : FModulate.family
        , modulateScale : FModulateScale.family
        , modulatePixelate : FModulatePixelate.family
        , modulateRotate : FModulateRotate.family
        , modulateHue : FModulateHue.family
        , render : FRender.family
        , update : FUpdate.family
        , setResolution : FSetResolution.family
        , hush : FHush.family
        , setFunction : FSetFunction.family
        , speed : FSpeed.family
        , bpm : FBpm.family
        , width : FWidth.family
        , height : FHeight.family
        , pi : FPi.family
        , time : FTime.family
        , mouse : FMouse.family
        , initCam : FInitCam.family
        , initImage : FInitImage.family
        , initVideo : FInitVideo.family
        , init : FInit.family
        , initStream : FInitStream.family
        , initScreen : FInitScreen.family
        , fast : FFast.family
        , smooth : FSmooth.family
        , ease : FEase.family
        , offset : FOffset.family
        , fit : FFit.family
        , fft : FFft.family
        , setSmooth : FSetSmooth.family
        , setCutoff : FSetCutoff.family
        , setBins : FSetBins.family
        , setScale : FSetScale.family
        , hide : FHide.family
        , show : FShow.family
        , out : FOut.family
        }


type Toolkit m = HydraToolkit m