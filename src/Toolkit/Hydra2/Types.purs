module Toolkit.Hydra2.Types where

import Prelude

import Effect (Effect)

import Color (Color)
import Color (rgb, black) as Color
import Cli.Palette.Set.X11 as X11

import Data.Maybe (fromMaybe)
import Data.Mark (class Mark)


data TODO = TODO


newtype Context =
    Context
        { time :: Number
        -- , ...
        }


data Value
    = None
    | Required -- a.k.a. Undefined
    | Number Number
    | VArray Values Ease
    | Dep (Context -> Number)
    -- | ...
    | Time
    | MouseX
    | MouseY
    | Width
    | Height
    | Pi
    -- | ...
    | Audio Audio AudioBin


newtype Values = Values (Array Value)


data Texture
    = Empty
    | From Source
    | BlendOf { what :: Texture, with :: Texture } Blend
    | WithColor Texture ColorOp
    | ModulateWith { what :: Texture, with :: Texture } Modulate
    | Geometry Texture Geometry


data Source
    = Dynamic
    | Video
    | Camera
    | Gradient { speed :: Value }
    | Noise { scale :: Value, offset :: Value }
    | Osc { frequency :: Value, sync :: Value, offset :: Value }
    | Shape { sides :: Value, radius :: Value, smoothing :: Value }
    | Solid { r :: Value, g :: Value, b :: Value, a :: Value }
    | Source From -- Output?
    | Voronoi { scale :: Value, speed :: Value, blending :: Value }
    -- | ..


data Blend
    = Blend Value -- amount
    | Add Value -- amount
    | Diff
    | Layer Value -- amount
    | Mask
    | Mult Value -- amount
    | Sub Value -- amount


data ColorOp
    = R { scale :: Value, offset :: Value }
    | G { scale :: Value, offset :: Value }
    | B { scale :: Value, offset :: Value }
    | A { scale :: Value, offset :: Value }
    | Posterize { bins :: Value, gamma :: Value }
    | Shift { r :: Value, g :: Value, b :: Value, a :: Value }
    | Invert Value -- amount
    | Contrast Value -- amount
    | Brightness Value -- amount
    | Luma { treshold :: Value, tolerance :: Value }
    | Tresh { treshold :: Value, tolerance :: Value }
    | Color { r :: Value, g :: Value, b :: Value, a :: Value }
    | Saturate Value -- amount
    | Hue Value -- amount
    | Colorama Value -- amount


data Modulate
    = Modulate Value -- amount
    | ModHue Value -- amount
    | ModKaleid { nSides :: Value }
    | ModPixelate { multiple :: Value, offset :: Value }
    | ModRepeat { offsetX :: Value, offsetY :: Value, repeatX :: Value, repeatY :: Value }
    | ModRepeatX { offset :: Value, reps :: Value } -- TODO: join with `ModRepeat`
    | ModRepeatY { offset :: Value, reps :: Value } -- TODO: join with `ModRepeat`
    | ModRotate { multiple :: Value, offset :: Value }
    | ModScale { multiple :: Value, offset :: Value }
    | ModScroll { scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value }
    | ModScrollX { scrollX :: Value, speed :: Value } -- TODO: join with `Scroll`
    | ModScrollY { scrollY :: Value, speed :: Value } -- TODO: join with `Scroll`


data Geometry
    = GKaleid { nSides :: Value }
    | GPixelate { pixelX :: Value, pixelY :: Value }
    | GRepeat { offsetX :: Value, offsetY :: Value, repeatX :: Value, repeatY :: Value }
    | GRepeatX { offset :: Value, reps :: Value } -- TODO: join with `Repeat`
    | GRepeatY { offset :: Value, reps :: Value } -- TODO: join with `Repeat`
    | GRotate { angle :: Value, speed :: Value }
    | GScale { amount :: Value, xMult :: Value, yMult :: Value, offsetX :: Value, offsetY :: Value }
    | GScroll { scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value }
    | GScrollX { scrollX :: Value, speed :: Value } -- TODO: join with `Scroll`
    | GScrollY { scrollY :: Value, speed :: Value } -- TODO: join with `Scroll`


data Output
    = Screen
    | Output0
    | Output1
    | Output2
    -- | ...


data Ease
    = Linear
    | Fast Value -- amount
    | Smooth Value -- amount
    | Fit { low :: Value, high :: Value }
    | Offset Value -- amount
    | InOutCubic
    -- | ...


data From
    = All
    | Output Output


data Audio
    = Silence
    | Mic
    | File
    -- | ...

data AudioBin
    = H0
    | H1
    | H2
    | H3
    | H4
    -- ..

newtype UpdateFn = UpdateFn (Context -> Effect Unit)


data Canvas = Canvas


newtype SourceOptions =
    SourceOptions
        { src :: Canvas
        -- , ...
        }


defaultSourceOptions :: SourceOptions
defaultSourceOptions =
    SourceOptions
        { src : Canvas }


newtype GlslFn = GlslFn Unit


newtype Url = Url String


noUrl :: Url
noUrl = Url ""


defaultGlslFn :: GlslFn
defaultGlslFn = GlslFn unit


defaultUpdateFn :: UpdateFn
defaultUpdateFn = UpdateFn $ const $ pure unit


noValues :: Values
noValues = Values []


defaultSource :: Source
defaultSource = Source All





{- MARK -}


instance Mark Value where
    mark :: Value -> Color
    mark = const $ Color.rgb 155 205 155


instance Mark Texture where
    mark :: Texture -> Color
    mark = const $ fromMaybe Color.black X11.darkorange.color
    -- mark = const $ Color.rgb 148 0 211


instance Mark From where
    mark :: From -> Color
    mark = const $ Color.rgb 205 16 118


instance Mark TODO where
    mark :: TODO -> Color
    mark = const $ Color.rgb 24 116 205


instance Mark Context where
    mark :: Context -> Color
    mark = const $ Color.rgb 238 201 0


instance Mark UpdateFn where
    mark :: UpdateFn -> Color
    mark = const $ Color.rgb 219 112 147


instance Mark Source where
    mark :: Source -> Color
    mark = const $ Color.rgb 238 154 73


instance Mark Url where
    mark :: Url -> Color
    mark = const $ Color.rgb 150 205 205


instance Mark GlslFn where
    mark :: GlslFn -> Color
    mark = const $ Color.rgb 139 137 137


instance Mark SourceOptions where
    mark :: SourceOptions -> Color
    mark = const $ Color.rgb 139 102 139


instance Mark Values where
    mark :: Values -> Color
    mark = const $ Color.rgb 205 181 205


instance Mark Ease where
    mark :: Ease -> Color
    mark = const $ Color.rgb 240 128 128


instance Mark Audio where
    mark :: Audio -> Color
    mark = const $ Color.rgb 173 255 47


instance Mark AudioBin where
    mark :: AudioBin -> Color
    mark = const $ Color.rgb 238 230 133


instance Mark Output where
    mark :: Output -> Color
    mark = const $ Color.rgb 250 250 205



    {-
    mark = case _ of
        Value _ -> X11.lightyellow -- X11.seagreen-- mark HG.Synth
        Unit _ -> X11.lightgray
        Texture _ -> X11.darkorange
        From _ -> X11.limegreen
        TODO _ -> X11.burlywood
        Context _ -> X11.papayawhip
        UpdateFn _ -> X11.salmon
        Source _ -> X11.cornsilk
        Url _ -> X11.cornflowerblue
        GlslFn _ -> X11.crimson
        SourceOptions _ -> X11.palevioletred
        Values _ -> mark HG.Array
        Ease _ -> X11.darkgoldenrod
        Audio _ -> mark HG.Audio
        AudioBin _ -> X11.aqua
        Output _ -> X11.blue
    -}