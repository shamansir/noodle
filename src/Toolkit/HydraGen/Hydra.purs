module Hydra where

import Toolkit.Families.MNoise as MNoise
import Toolkit.Families.MVoronoi as MVoronoi
import Toolkit.Families.MOsc as MOsc
import Toolkit.Families.MShape as MShape
import Toolkit.Families.MGradient as MGradient
import Toolkit.Families.MSrc as MSrc
import Toolkit.Families.MSolid as MSolid
import Toolkit.Families.MSrc as MSrc
import Toolkit.Families.MPrev as MPrev
import Toolkit.Families.MRotate as MRotate
import Toolkit.Families.MScale as MScale
import Toolkit.Families.MPixelate as MPixelate
import Toolkit.Families.MRepeat as MRepeat
import Toolkit.Families.MRepeatX as MRepeatX
import Toolkit.Families.MRepeatY as MRepeatY
import Toolkit.Families.MKaleid as MKaleid
import Toolkit.Families.MScroll as MScroll
import Toolkit.Families.MScrollX as MScrollX
import Toolkit.Families.MScrollY as MScrollY
import Toolkit.Families.MPosterize as MPosterize
import Toolkit.Families.MShift as MShift
import Toolkit.Families.MInvert as MInvert
import Toolkit.Families.MContrast as MContrast
import Toolkit.Families.MBrightness as MBrightness
import Toolkit.Families.MLuma as MLuma
import Toolkit.Families.MTresh as MTresh
import Toolkit.Families.MColor as MColor
import Toolkit.Families.MSaturate as MSaturate
import Toolkit.Families.MHue as MHue
import Toolkit.Families.MColorama as MColorama
import Toolkit.Families.MSum as MSum
import Toolkit.Families.MR as MR
import Toolkit.Families.MG as MG
import Toolkit.Families.MB as MB
import Toolkit.Families.MA as MA
import Toolkit.Families.MAdd as MAdd
import Toolkit.Families.MSub as MSub
import Toolkit.Families.MLayer as MLayer
import Toolkit.Families.MBlend as MBlend
import Toolkit.Families.MMult as MMult
import Toolkit.Families.MDiff as MDiff
import Toolkit.Families.MMask as MMask
import Toolkit.Families.MModulateRepeat as MModulateRepeat
import Toolkit.Families.MModulateRepeatX as MModulateRepeatX
import Toolkit.Families.MModulateRepeatY as MModulateRepeatY
import Toolkit.Families.MModulateKaleid as MModulateKaleid
import Toolkit.Families.MModulateScrollX as MModulateScrollX
import Toolkit.Families.MModulateScrollY as MModulateScrollY
import Toolkit.Families.MModulate as MModulate
import Toolkit.Families.MModulateScale as MModulateScale
import Toolkit.Families.MModulatePixelate as MModulatePixelate
import Toolkit.Families.MModulateRotate as MModulateRotate
import Toolkit.Families.MModulateHue as MModulateHue
import Toolkit.Families.MRender as MRender
import Toolkit.Families.MUpdate as MUpdate
import Toolkit.Families.MSetResolution as MSetResolution
import Toolkit.Families.MHush as MHush
import Toolkit.Families.MSetFunction as MSetFunction
import Toolkit.Families.MSpeed as MSpeed
import Toolkit.Families.MBpm as MBpm
import Toolkit.Families.MWidth as MWidth
import Toolkit.Families.MHeight as MHeight
import Toolkit.Families.MPi as MPi
import Toolkit.Families.MTime as MTime
import Toolkit.Families.MMouse as MMouse
import Toolkit.Families.MInitCam as MInitCam
import Toolkit.Families.MInitImage as MInitImage
import Toolkit.Families.MInitVideo as MInitVideo
import Toolkit.Families.MInit as MInit
import Toolkit.Families.MInitStream as MInitStream
import Toolkit.Families.MInitScreen as MInitScreen
import Toolkit.Families.MFast as MFast
import Toolkit.Families.MSmooth as MSmooth
import Toolkit.Families.MEase as MEase
import Toolkit.Families.MOffset as MOffset
import Toolkit.Families.MFit as MFit
import Toolkit.Families.MFft as MFft
import Toolkit.Families.MSetSmooth as MSetSmooth
import Toolkit.Families.MSetCutoff as MSetCutoff
import Toolkit.Families.MSetBins as MSetBins
import Toolkit.Families.MSetScale as MSetScale
import Toolkit.Families.MHide as MHide
import Toolkit.Families.MShow as MShow
import Toolkit.Families.MOut as MOut

type Toolkit m
    = Toolkit Unit
        ( noise :: MNoise.Family -- {-> source <-}
        , voronoi :: MVoronoi.Family -- {-> source <-}
        , osc :: MOsc.Family -- {-> source <-}
        , shape :: MShape.Family -- {-> source <-}
        , gradient :: MGradient.Family -- {-> source <-}
        , src :: MSrc.Family -- {-> source <-}
        , solid :: MSolid.Family -- {-> source <-}
        , src :: MSrc.Family -- {-> source <-}
        , prev :: MPrev.Family -- {-> source <-}
        , rotate :: MRotate.Family -- {-> geometry <-}
        , scale :: MScale.Family -- {-> geometry <-}
        , pixelate :: MPixelate.Family -- {-> geometry <-}
        , repeat :: MRepeat.Family -- {-> geometry <-}
        , repeatX :: MRepeatX.Family -- {-> geometry <-}
        , repeatY :: MRepeatY.Family -- {-> geometry <-}
        , kaleid :: MKaleid.Family -- {-> geometry <-}
        , scroll :: MScroll.Family -- {-> geometry <-}
        , scrollX :: MScrollX.Family -- {-> geometry <-}
        , scrollY :: MScrollY.Family -- {-> geometry <-}
        , posterize :: MPosterize.Family -- {-> color <-}
        , shift :: MShift.Family -- {-> color <-}
        , invert :: MInvert.Family -- {-> color <-}
        , contrast :: MContrast.Family -- {-> color <-}
        , brightness :: MBrightness.Family -- {-> color <-}
        , luma :: MLuma.Family -- {-> color <-}
        , tresh :: MTresh.Family -- {-> color <-}
        , color :: MColor.Family -- {-> color <-}
        , saturate :: MSaturate.Family -- {-> color <-}
        , hue :: MHue.Family -- {-> color <-}
        , colorama :: MColorama.Family -- {-> color <-}
        , sum :: MSum.Family -- {-> color <-}
        , r :: MR.Family -- {-> color <-}
        , g :: MG.Family -- {-> color <-}
        , b :: MB.Family -- {-> color <-}
        , a :: MA.Family -- {-> color <-}
        , add :: MAdd.Family -- {-> blend <-}
        , sub :: MSub.Family -- {-> blend <-}
        , layer :: MLayer.Family -- {-> blend <-}
        , blend :: MBlend.Family -- {-> blend <-}
        , mult :: MMult.Family -- {-> blend <-}
        , diff :: MDiff.Family -- {-> blend <-}
        , mask :: MMask.Family -- {-> blend <-}
        , modulateRepeat :: MModulateRepeat.Family -- {-> modulate <-}
        , modulateRepeatX :: MModulateRepeatX.Family -- {-> modulate <-}
        , modulateRepeatY :: MModulateRepeatY.Family -- {-> modulate <-}
        , modulateKaleid :: MModulateKaleid.Family -- {-> modulate <-}
        , modulateScrollX :: MModulateScrollX.Family -- {-> modulate <-}
        , modulateScrollY :: MModulateScrollY.Family -- {-> modulate <-}
        , modulate :: MModulate.Family -- {-> modulate <-}
        , modulateScale :: MModulateScale.Family -- {-> modulate <-}
        , modulatePixelate :: MModulatePixelate.Family -- {-> modulate <-}
        , modulateRotate :: MModulateRotate.Family -- {-> modulate <-}
        , modulateHue :: MModulateHue.Family -- {-> modulate <-}
        , render :: MRender.Family -- {-> synth <-}
        , update :: MUpdate.Family -- {-> synth <-}
        , setResolution :: MSetResolution.Family -- {-> synth <-}
        , hush :: MHush.Family -- {-> synth <-}
        , setFunction :: MSetFunction.Family -- {-> synth <-}
        , speed :: MSpeed.Family -- {-> synth <-}
        , bpm :: MBpm.Family -- {-> synth <-}
        , width :: MWidth.Family -- {-> synth <-}
        , height :: MHeight.Family -- {-> synth <-}
        , pi :: MPi.Family -- {-> synth <-}
        , time :: MTime.Family -- {-> synth <-}
        , mouse :: MMouse.Family -- {-> synth <-}
        , initCam :: MInitCam.Family -- {-> extsource <-}
        , initImage :: MInitImage.Family -- {-> extsource <-}
        , initVideo :: MInitVideo.Family -- {-> extsource <-}
        , init :: MInit.Family -- {-> extsource <-}
        , initStream :: MInitStream.Family -- {-> extsource <-}
        , initScreen :: MInitScreen.Family -- {-> extsource <-}
        , fast :: MFast.Family -- {-> array <-}
        , smooth :: MSmooth.Family -- {-> array <-}
        , ease :: MEase.Family -- {-> array <-}
        , offset :: MOffset.Family -- {-> array <-}
        , fit :: MFit.Family -- {-> array <-}
        , fft :: MFft.Family -- {-> audio <-}
        , setSmooth :: MSetSmooth.Family -- {-> audio <-}
        , setCutoff :: MSetCutoff.Family -- {-> audio <-}
        , setBins :: MSetBins.Family -- {-> audio <-}
        , setScale :: MSetScale.Family -- {-> audio <-}
        , hide :: MHide.Family -- {-> audio <-}
        , show :: MShow.Family -- {-> audio <-}
        , out :: MOut.Family -- {-> out <-}
        )