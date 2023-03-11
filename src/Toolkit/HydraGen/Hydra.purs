module Toolkit.HydraGen where

import Toolkit.Families.Source.MNoise as Source.MNoise
import Toolkit.Families.Source.MVoronoi as Source.MVoronoi
import Toolkit.Families.Source.MOsc as Source.MOsc
import Toolkit.Families.Source.MShape as Source.MShape
import Toolkit.Families.Source.MGradient as Source.MGradient
import Toolkit.Families.Source.MSrc as Source.MSrc
import Toolkit.Families.Source.MSolid as Source.MSolid
import Toolkit.Families.Source.MSrc as Source.MSrc
import Toolkit.Families.Source.MPrev as Source.MPrev
import Toolkit.Families.Geometry.MRotate as Geometry.MRotate
import Toolkit.Families.Geometry.MScale as Geometry.MScale
import Toolkit.Families.Geometry.MPixelate as Geometry.MPixelate
import Toolkit.Families.Geometry.MRepeat as Geometry.MRepeat
import Toolkit.Families.Geometry.MRepeatX as Geometry.MRepeatX
import Toolkit.Families.Geometry.MRepeatY as Geometry.MRepeatY
import Toolkit.Families.Geometry.MKaleid as Geometry.MKaleid
import Toolkit.Families.Geometry.MScroll as Geometry.MScroll
import Toolkit.Families.Geometry.MScrollX as Geometry.MScrollX
import Toolkit.Families.Geometry.MScrollY as Geometry.MScrollY
import Toolkit.Families.Color.MPosterize as Color.MPosterize
import Toolkit.Families.Color.MShift as Color.MShift
import Toolkit.Families.Color.MInvert as Color.MInvert
import Toolkit.Families.Color.MContrast as Color.MContrast
import Toolkit.Families.Color.MBrightness as Color.MBrightness
import Toolkit.Families.Color.MLuma as Color.MLuma
import Toolkit.Families.Color.MTresh as Color.MTresh
import Toolkit.Families.Color.MColor as Color.MColor
import Toolkit.Families.Color.MSaturate as Color.MSaturate
import Toolkit.Families.Color.MHue as Color.MHue
import Toolkit.Families.Color.MColorama as Color.MColorama
import Toolkit.Families.Color.MSum as Color.MSum
import Toolkit.Families.Color.MR as Color.MR
import Toolkit.Families.Color.MG as Color.MG
import Toolkit.Families.Color.MB as Color.MB
import Toolkit.Families.Color.MA as Color.MA
import Toolkit.Families.Blend.MAdd as Blend.MAdd
import Toolkit.Families.Blend.MSub as Blend.MSub
import Toolkit.Families.Blend.MLayer as Blend.MLayer
import Toolkit.Families.Blend.MBlend as Blend.MBlend
import Toolkit.Families.Blend.MMult as Blend.MMult
import Toolkit.Families.Blend.MDiff as Blend.MDiff
import Toolkit.Families.Blend.MMask as Blend.MMask
import Toolkit.Families.Modulate.MModulateRepeat as Modulate.MModulateRepeat
import Toolkit.Families.Modulate.MModulateRepeatX as Modulate.MModulateRepeatX
import Toolkit.Families.Modulate.MModulateRepeatY as Modulate.MModulateRepeatY
import Toolkit.Families.Modulate.MModulateKaleid as Modulate.MModulateKaleid
import Toolkit.Families.Modulate.MModulateScrollX as Modulate.MModulateScrollX
import Toolkit.Families.Modulate.MModulateScrollY as Modulate.MModulateScrollY
import Toolkit.Families.Modulate.MModulate as Modulate.MModulate
import Toolkit.Families.Modulate.MModulateScale as Modulate.MModulateScale
import Toolkit.Families.Modulate.MModulatePixelate as Modulate.MModulatePixelate
import Toolkit.Families.Modulate.MModulateRotate as Modulate.MModulateRotate
import Toolkit.Families.Modulate.MModulateHue as Modulate.MModulateHue
import Toolkit.Families.Synth.MRender as Synth.MRender
import Toolkit.Families.Synth.MUpdate as Synth.MUpdate
import Toolkit.Families.Synth.MSetResolution as Synth.MSetResolution
import Toolkit.Families.Synth.MHush as Synth.MHush
import Toolkit.Families.Synth.MSetFunction as Synth.MSetFunction
import Toolkit.Families.Synth.MSpeed as Synth.MSpeed
import Toolkit.Families.Synth.MBpm as Synth.MBpm
import Toolkit.Families.Synth.MWidth as Synth.MWidth
import Toolkit.Families.Synth.MHeight as Synth.MHeight
import Toolkit.Families.Synth.MPi as Synth.MPi
import Toolkit.Families.Synth.MTime as Synth.MTime
import Toolkit.Families.Synth.MMouse as Synth.MMouse
import Toolkit.Families.Extsource.MInitCam as Extsource.MInitCam
import Toolkit.Families.Extsource.MInitImage as Extsource.MInitImage
import Toolkit.Families.Extsource.MInitVideo as Extsource.MInitVideo
import Toolkit.Families.Extsource.MInit as Extsource.MInit
import Toolkit.Families.Extsource.MInitStream as Extsource.MInitStream
import Toolkit.Families.Extsource.MInitScreen as Extsource.MInitScreen
import Toolkit.Families.Array.MFast as Array.MFast
import Toolkit.Families.Array.MSmooth as Array.MSmooth
import Toolkit.Families.Array.MEase as Array.MEase
import Toolkit.Families.Array.MOffset as Array.MOffset
import Toolkit.Families.Array.MFit as Array.MFit
import Toolkit.Families.Audio.MFft as Audio.MFft
import Toolkit.Families.Audio.MSetSmooth as Audio.MSetSmooth
import Toolkit.Families.Audio.MSetCutoff as Audio.MSetCutoff
import Toolkit.Families.Audio.MSetBins as Audio.MSetBins
import Toolkit.Families.Audio.MSetScale as Audio.MSetScale
import Toolkit.Families.Audio.MHide as Audio.MHide
import Toolkit.Families.Audio.MShow as Audio.MShow
import Toolkit.Families.Out.MOut as Out.MOut


type Toolkit m
    = Toolkit Unit
        ( noise :: Source.MNoise.Family -- {-> source <-}
        , voronoi :: Source.MVoronoi.Family -- {-> source <-}
        , osc :: Source.MOsc.Family -- {-> source <-}
        , shape :: Source.MShape.Family -- {-> source <-}
        , gradient :: Source.MGradient.Family -- {-> source <-}
        , src :: Source.MSrc.Family -- {-> source <-}
        , solid :: Source.MSolid.Family -- {-> source <-}
        , src :: Source.MSrc.Family -- {-> source <-}
        , prev :: Source.MPrev.Family -- {-> source <-}
        , rotate :: Geometry.MRotate.Family -- {-> geometry <-}
        , scale :: Geometry.MScale.Family -- {-> geometry <-}
        , pixelate :: Geometry.MPixelate.Family -- {-> geometry <-}
        , repeat :: Geometry.MRepeat.Family -- {-> geometry <-}
        , repeatX :: Geometry.MRepeatX.Family -- {-> geometry <-}
        , repeatY :: Geometry.MRepeatY.Family -- {-> geometry <-}
        , kaleid :: Geometry.MKaleid.Family -- {-> geometry <-}
        , scroll :: Geometry.MScroll.Family -- {-> geometry <-}
        , scrollX :: Geometry.MScrollX.Family -- {-> geometry <-}
        , scrollY :: Geometry.MScrollY.Family -- {-> geometry <-}
        , posterize :: Color.MPosterize.Family -- {-> color <-}
        , shift :: Color.MShift.Family -- {-> color <-}
        , invert :: Color.MInvert.Family -- {-> color <-}
        , contrast :: Color.MContrast.Family -- {-> color <-}
        , brightness :: Color.MBrightness.Family -- {-> color <-}
        , luma :: Color.MLuma.Family -- {-> color <-}
        , tresh :: Color.MTresh.Family -- {-> color <-}
        , color :: Color.MColor.Family -- {-> color <-}
        , saturate :: Color.MSaturate.Family -- {-> color <-}
        , hue :: Color.MHue.Family -- {-> color <-}
        , colorama :: Color.MColorama.Family -- {-> color <-}
        , sum :: Color.MSum.Family -- {-> color <-}
        , r :: Color.MR.Family -- {-> color <-}
        , g :: Color.MG.Family -- {-> color <-}
        , b :: Color.MB.Family -- {-> color <-}
        , a :: Color.MA.Family -- {-> color <-}
        , add :: Blend.MAdd.Family -- {-> blend <-}
        , sub :: Blend.MSub.Family -- {-> blend <-}
        , layer :: Blend.MLayer.Family -- {-> blend <-}
        , blend :: Blend.MBlend.Family -- {-> blend <-}
        , mult :: Blend.MMult.Family -- {-> blend <-}
        , diff :: Blend.MDiff.Family -- {-> blend <-}
        , mask :: Blend.MMask.Family -- {-> blend <-}
        , modulateRepeat :: Modulate.MModulateRepeat.Family -- {-> modulate <-}
        , modulateRepeatX :: Modulate.MModulateRepeatX.Family -- {-> modulate <-}
        , modulateRepeatY :: Modulate.MModulateRepeatY.Family -- {-> modulate <-}
        , modulateKaleid :: Modulate.MModulateKaleid.Family -- {-> modulate <-}
        , modulateScrollX :: Modulate.MModulateScrollX.Family -- {-> modulate <-}
        , modulateScrollY :: Modulate.MModulateScrollY.Family -- {-> modulate <-}
        , modulate :: Modulate.MModulate.Family -- {-> modulate <-}
        , modulateScale :: Modulate.MModulateScale.Family -- {-> modulate <-}
        , modulatePixelate :: Modulate.MModulatePixelate.Family -- {-> modulate <-}
        , modulateRotate :: Modulate.MModulateRotate.Family -- {-> modulate <-}
        , modulateHue :: Modulate.MModulateHue.Family -- {-> modulate <-}
        , render :: Synth.MRender.Family -- {-> synth <-}
        , update :: Synth.MUpdate.Family -- {-> synth <-}
        , setResolution :: Synth.MSetResolution.Family -- {-> synth <-}
        , hush :: Synth.MHush.Family -- {-> synth <-}
        , setFunction :: Synth.MSetFunction.Family -- {-> synth <-}
        , speed :: Synth.MSpeed.Family -- {-> synth <-}
        , bpm :: Synth.MBpm.Family -- {-> synth <-}
        , width :: Synth.MWidth.Family -- {-> synth <-}
        , height :: Synth.MHeight.Family -- {-> synth <-}
        , pi :: Synth.MPi.Family -- {-> synth <-}
        , time :: Synth.MTime.Family -- {-> synth <-}
        , mouse :: Synth.MMouse.Family -- {-> synth <-}
        , initCam :: Extsource.MInitCam.Family -- {-> extsource <-}
        , initImage :: Extsource.MInitImage.Family -- {-> extsource <-}
        , initVideo :: Extsource.MInitVideo.Family -- {-> extsource <-}
        , init :: Extsource.MInit.Family -- {-> extsource <-}
        , initStream :: Extsource.MInitStream.Family -- {-> extsource <-}
        , initScreen :: Extsource.MInitScreen.Family -- {-> extsource <-}
        , fast :: Array.MFast.Family -- {-> array <-}
        , smooth :: Array.MSmooth.Family -- {-> array <-}
        , ease :: Array.MEase.Family -- {-> array <-}
        , offset :: Array.MOffset.Family -- {-> array <-}
        , fit :: Array.MFit.Family -- {-> array <-}
        , fft :: Audio.MFft.Family -- {-> audio <-}
        , setSmooth :: Audio.MSetSmooth.Family -- {-> audio <-}
        , setCutoff :: Audio.MSetCutoff.Family -- {-> audio <-}
        , setBins :: Audio.MSetBins.Family -- {-> audio <-}
        , setScale :: Audio.MSetScale.Family -- {-> audio <-}
        , hide :: Audio.MHide.Family -- {-> audio <-}
        , show :: Audio.MShow.Family -- {-> audio <-}
        , out :: Out.MOut.Family -- {-> out <-}
        )

toolkit =
    Toolkit.from "hydra"
        { noise : Source.MNoise.family
        , voronoi : Source.MVoronoi.family
        , osc : Source.MOsc.family
        , shape : Source.MShape.family
        , gradient : Source.MGradient.family
        , src : Source.MSrc.family
        , solid : Source.MSolid.family
        , src : Source.MSrc.family
        , prev : Source.MPrev.family
        , rotate : Geometry.MRotate.family
        , scale : Geometry.MScale.family
        , pixelate : Geometry.MPixelate.family
        , repeat : Geometry.MRepeat.family
        , repeatX : Geometry.MRepeatX.family
        , repeatY : Geometry.MRepeatY.family
        , kaleid : Geometry.MKaleid.family
        , scroll : Geometry.MScroll.family
        , scrollX : Geometry.MScrollX.family
        , scrollY : Geometry.MScrollY.family
        , posterize : Color.MPosterize.family
        , shift : Color.MShift.family
        , invert : Color.MInvert.family
        , contrast : Color.MContrast.family
        , brightness : Color.MBrightness.family
        , luma : Color.MLuma.family
        , tresh : Color.MTresh.family
        , color : Color.MColor.family
        , saturate : Color.MSaturate.family
        , hue : Color.MHue.family
        , colorama : Color.MColorama.family
        , sum : Color.MSum.family
        , r : Color.MR.family
        , g : Color.MG.family
        , b : Color.MB.family
        , a : Color.MA.family
        , add : Blend.MAdd.family
        , sub : Blend.MSub.family
        , layer : Blend.MLayer.family
        , blend : Blend.MBlend.family
        , mult : Blend.MMult.family
        , diff : Blend.MDiff.family
        , mask : Blend.MMask.family
        , modulateRepeat : Modulate.MModulateRepeat.family
        , modulateRepeatX : Modulate.MModulateRepeatX.family
        , modulateRepeatY : Modulate.MModulateRepeatY.family
        , modulateKaleid : Modulate.MModulateKaleid.family
        , modulateScrollX : Modulate.MModulateScrollX.family
        , modulateScrollY : Modulate.MModulateScrollY.family
        , modulate : Modulate.MModulate.family
        , modulateScale : Modulate.MModulateScale.family
        , modulatePixelate : Modulate.MModulatePixelate.family
        , modulateRotate : Modulate.MModulateRotate.family
        , modulateHue : Modulate.MModulateHue.family
        , render : Synth.MRender.family
        , update : Synth.MUpdate.family
        , setResolution : Synth.MSetResolution.family
        , hush : Synth.MHush.family
        , setFunction : Synth.MSetFunction.family
        , speed : Synth.MSpeed.family
        , bpm : Synth.MBpm.family
        , width : Synth.MWidth.family
        , height : Synth.MHeight.family
        , pi : Synth.MPi.family
        , time : Synth.MTime.family
        , mouse : Synth.MMouse.family
        , initCam : Extsource.MInitCam.family
        , initImage : Extsource.MInitImage.family
        , initVideo : Extsource.MInitVideo.family
        , init : Extsource.MInit.family
        , initStream : Extsource.MInitStream.family
        , initScreen : Extsource.MInitScreen.family
        , fast : Array.MFast.family
        , smooth : Array.MSmooth.family
        , ease : Array.MEase.family
        , offset : Array.MOffset.family
        , fit : Array.MFit.family
        , fft : Audio.MFft.family
        , setSmooth : Audio.MSetSmooth.family
        , setCutoff : Audio.MSetCutoff.family
        , setBins : Audio.MSetBins.family
        , setScale : Audio.MSetScale.family
        , hide : Audio.MHide.family
        , show : Audio.MShow.family
        , out : Out.MOut.family
        }