module HydraTk.Types where

import Effect (Effect)

import Prelude

import Type.Proxy (Proxy(..))

import Data.Newtype (class Newtype, unwrap, wrap)

import Color (Color)
import Color (black, rgb) as Color

import Data.Array ((:))
import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith) as String
import Data.String.Extra (pascalCase) as String
import Data.Tuple.Nested (type (/\), (/\))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Functor.Extra ((<$$>))
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)

import PureScript.CST.Types as CST
import Tidy.Codegen

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Fn.Signature (Signature(..), class ToSignature)
import Noodle.Fn.Signature (Argument, Output, empty, sig', toPureScript, toJavaScript, arg, filter, argValue) as Sig
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ValueCodegen, mkExpression)
import Noodle.Ui.Palette.Mark (class Mark, mark)
import Noodle.Ui.Palette.Item (colorOf) as C
import Noodle.Ui.Palette.Set.X11 as X11

import Noodle.Text.ToCode (class ToCode, toPureScript, toPureScript', toJavaScript, toJavaScript')
import Noodle.Text.Code.Target (PS, JS, pureScript, javaScript)



{-
instance Read a => Decode a where
    decode = read
-}


data TODO = TODO


newtype Context =
    Context
        { time :: Number
        , mouseX :: Number
        , mouseY :: Number
        -- , audio :: Audio
        , fft :: (AudioBin -> Number)
        , bins :: Int
        , width :: Int
        , height :: Int
        }


data Value
    = None
    | Undefined
    | Number Number
    | VArray Values Ease
    | Dep DepFn
    | Time
    | MouseX
    | MouseY
    | Width
    | Height
    | Pi
    | Fft AudioBin -- add AudioSource back ?
    -- TODO: Glsl


derive instance Eq EaseType
derive instance Eq Ease
derive instance Eq Values
derive instance Eq Value
derive instance Eq JsExpr
derive instance Eq AudioSource
derive instance Eq AudioBin
-- derive instance Eq Texture
-- derive instance Eq Source
-- derive instance Eq Blend
-- derive instance Eq ColorOp
-- derive instance Eq Modulate
-- derive instance Eq Geometry
-- derive instance Eq Ease
derive instance Eq OutputN


newtype Values = Values (Array Value)


data JsExpr
    = Val Value
    | DivE JsExpr JsExpr
    | MulE JsExpr JsExpr
    | SubE JsExpr JsExpr
    | AddE JsExpr JsExpr
    | ModE JsExpr JsExpr
    | Math String (Maybe JsExpr)
    | Brackets JsExpr


data DepFn
    = UserExpr JsExpr
    | DepFn (Context -> Effect Value)
    | Unparsed String
    | NoAction


instance Eq DepFn where
    eq :: DepFn -> DepFn -> Boolean
    eq (UserExpr jsExprA) (UserExpr jsExprB) = jsExprA == jsExprB
    eq (DepFn _) (DepFn _) = false -- could be fixed?
    eq (Unparsed strA) (Unparsed strB) = strA == strB
    eq NoAction NoAction = true
    eq _ _ = false


data Texture
    = Empty
    | Start Source -- start the chain
    | BlendOf { what :: Texture, with :: Texture } Blend
    | Filter Texture ColorOp
    | ModulateWith { what :: Texture, with :: Texture } Modulate
    | Geometry Texture Geometry
    | CallGlslFn { over :: Texture, mbWith :: Maybe Texture } GlslFnRef


-- TODO: Rethink naming since in Hydra `Texture` is only Noise / Osc / Shape / Solid / Voronoi / Src
--       The combination of `Texture` with modifications is rather Layer (there's `layer` function as well, what is `src` then?) with something
--       See also: src/Toolkit/Hydra2/glsl-functions.js
--       Maybe `From` is rather this `Layer`?

data From
    = Gradient { speed :: Value }
    | Noise { scale :: Value, offset :: Value }
    | Osc { frequency :: Value, sync :: Value, offset :: Value }
    | Shape { sides :: Value, radius :: Value, smoothing :: Value }
    | Solid { r :: Value, g :: Value, b :: Value, a :: Value }
    | Voronoi { scale :: Value, speed :: Value, blending :: Value }


data Source
    = From From
    | Load OutputN
    | External SourceN
    -- | ..


data ExtSource
    = Sketch String
    | Video
    | Camera Int
    -- TODO | Stream String
    | Unclear


data RenderTarget
    = Four
    | Output OutputN


newtype CanBeSource =
    CanBeSource (Either SourceN OutputN)


data SourceN
    = Source0
    -- | Source1


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
    | Luma { threshold :: Value, tolerance :: Value }
    | Thresh { threshold :: Value, tolerance :: Value }
    | Color { r :: Value, g :: Value, b :: Value, a :: Value }
    | Saturate Value -- amount
    | Hue Value -- amount
    | Colorama Value -- amount


data Modulate
    = Modulate Value -- amount
    | ModHue Value -- amount
    | ModKaleid { nSides :: Value }
    | ModPixelate { multiple :: Value, offset :: Value }
    | ModRepeat { repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value }
    | ModRepeatX { reps :: Value, offset :: Value } -- TODO: join with `ModRepeat`
    | ModRepeatY { reps :: Value, offset :: Value } -- TODO: join with `ModRepeat`
    | ModRotate { multiple :: Value, offset :: Value }
    | ModScale { multiple :: Value, offset :: Value }
    | ModScroll { scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value }
    | ModScrollX { scrollX :: Value, speed :: Value } -- TODO: join with `Scroll`
    | ModScrollY { scrollY :: Value, speed :: Value } -- TODO: join with `Scroll`


data Geometry
    = GKaleid { nSides :: Value }
    | GPixelate { pixelX :: Value, pixelY :: Value }
    | GRepeat { repeatX :: Value, repeatY :: Value, offsetX :: Value, offsetY :: Value }
    | GRepeatX { reps :: Value, offset :: Value } -- TODO: join with `Repeat`
    | GRepeatY { reps :: Value, offset :: Value } -- TODO: join with `Repeat`
    | GRotate { angle :: Value, speed :: Value }
    | GScale { amount :: Value, xMult :: Value, yMult :: Value, offsetX :: Value, offsetY :: Value }
    | GScroll { scrollX :: Value, scrollY :: Value, speedX :: Value, speedY :: Value }
    | GScrollX { scrollX :: Value, speed :: Value } -- TODO: join with `Scroll`
    | GScrollY { scrollY :: Value, speed :: Value } -- TODO: join with `Scroll`


data OutputN -- FIXME: Replace with newtype over Int? Bounded, for example
    = Output0
    | Output1
    | Output2
    | Output3
    | Output4 -- name them `OutputN` ?
    -- | ...


data EaseType -- TODO : `CSS.TimingFunction`
    = Linear
    | InOutCubic


data Ease
    = NoEase
    | Ease EaseType
    | Fast Value -- amount
    | Smooth Value -- amount
    | Fit { low :: Value, high :: Value }
    | Offset Value -- amount
    -- | ...


data AudioSource
    = Silence
    | Mic
    | File
    -- | ...


newtype AudioBin = AudioBin Int


newtype UpdateFn = UpdateFn (Context -> Effect Unit)


newtype TextureFn = TextureFn (Context -> Effect Texture) -- TODO: add as the Value option


data GlslFnKind
    = FnSrc
    | FnCoord
    | FnCombineCoord
    | FnCombine
    | FnColor


type GlslFnArg = TOrV


type GlslFnOut = Unit


newtype GlslFnCode = GlslFnCode String


-- FIXME: use `ToFn`
newtype GlslFn = GlslFn
    { kind :: GlslFnKind
    , code :: GlslFnCode
    , fn :: Signature GlslFnArg GlslFnOut  -- holds default value in every argument
    }


newtype GlslFnRef = GlslFnRef (Signature GlslFnArg GlslFnOut) -- should be the name of the function from the registry


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


newtype Url = Url String


data OnAudio
    = Show
    | SetBins Int
    | SetCutoff Number
    | SetScale Number
    | SetSmooth Number
    | Hide


data OnSynth
    = Render RenderTarget
    | Update UpdateFn
    | SetResolution Int Int
    | Hush
    | SetFunction GlslFnCode


data OnSource
    = Init SourceOptions
    | InitCam Int
    | InitImage Url
    | InitVideo Url
    -- InitStream
    | InitScreen
    | Clear


data SynthProp
    = Speed Number
    | Bpm Int
    | SetUpdateFn UpdateFn -- TODO


noUrl :: Url
noUrl = Url ""


defaultUpdateFn :: UpdateFn
defaultUpdateFn = UpdateFn $ const $ pure unit


defaultFn :: DepFn
defaultFn = NoAction


defaultTOrV :: TOrV
defaultTOrV = T Empty


defaultGlslFn :: GlslFn
defaultGlslFn = GlslFn { kind : FnSrc, code : GlslFnCode "", fn : Sig.empty "" }


defaultGlslFnRef :: GlslFnRef
defaultGlslFnRef = GlslFnRef $ Sig.empty ""


defaultGlslFnArg :: GlslFnArg
defaultGlslFnArg = defaultTOrV


noValues :: Values
noValues = Values []


defaultSourceN :: SourceN
defaultSourceN = Source0


defaultCanBeSource :: CanBeSource
defaultCanBeSource = CanBeSource $ Right Output0


initialContext :: Context
initialContext =
    Context
        { time : 0.0
        , mouseX : 0.0
        , mouseY : 0.0
        , fft : const 0.0
        , bins : 4
        , height : 0
        , width : 0
        }


{-
instance IsNodeState a Values where
    default = noValues
    fromGlobal = const Nothing


instance IsNodeState a Fn where
    default = NoAction
    fromGlobal = const Nothing


instance IsNodeState a OutputN where
    default = Output0
    fromGlobal = const Nothing


instance IsNodeState a CanBeSource where
    default = defaultCanBeSource
    fromGlobal = const Nothing
-}


{- HAS_FALLBACK -}

instance HasFallback Value where fallback = None
instance HasFallback Texture where fallback = Empty
instance HasFallback OutputN where fallback = Output0
instance HasFallback SourceN where fallback = Source0
instance HasFallback ExtSource where fallback = Unclear
instance HasFallback TODO where fallback = TODO
instance HasFallback Context where fallback = initialContext
instance HasFallback UpdateFn where fallback = defaultUpdateFn
instance HasFallback Url where fallback = noUrl
instance HasFallback GlslFn where fallback = defaultGlslFn
instance HasFallback SourceOptions where fallback = defaultSourceOptions
instance HasFallback Values where fallback = noValues
instance HasFallback Ease where fallback = Ease Linear
instance HasFallback AudioSource where fallback = Silence
instance HasFallback AudioBin where fallback = AudioBin 0

instance HasFallback Source where fallback = Load HF.fallback
instance HasFallback TOrV where fallback = V HF.fallback
instance HasFallback RenderTarget where fallback = Output HF.fallback -- TODO: Four ?
instance HasFallback DepFn where fallback = NoAction
instance HasFallback CanBeSource where fallback = defaultCanBeSource


{- MARK -}


instance Mark Value where
    mark :: Value -> Color
    mark = const $ Color.rgb 155 205 155


instance Mark Texture where
    mark :: Texture -> Color
    mark = const $ C.colorOf $ X11.darkorange
    -- mark = const $ Color.rgb 148 0 211


instance Mark ExtSource where
    mark :: ExtSource -> Color
    mark = const $ Color.rgb 205 16 118


instance Mark SourceN where
    mark :: SourceN -> Color
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


instance Mark AudioSource where
    mark :: AudioSource -> Color
    mark = const $ Color.rgb 173 255 47


instance Mark AudioBin where
    mark :: AudioBin -> Color
    mark = const $ Color.rgb 238 230 133


instance Mark OutputN where
    mark :: OutputN -> Color
    mark = const $ Color.rgb 250 250 205


instance Mark DepFn where
    mark :: DepFn -> Color
    mark = const $ Color.rgb 150 180 205


instance Mark RenderTarget where
    mark :: RenderTarget -> Color
    mark = case _ of
        Output _ -> Color.rgb 250 250 205
        Four -> Color.rgb 250 0 0


instance Mark CanBeSource where
    mark :: CanBeSource -> Color
    mark (CanBeSource cbs) = case cbs of
        Left _ -> Color.rgb 0 250 0
        Right _ -> Color.rgb 250 250 205


data TOrV
    = T Texture
    | V Value


data OTOrV
    = OT
    | OV


data EOrV
    = E EaseType
    | EV Value


data FnArg
    = TArg Texture
    | VArg Value
    | UrlArg Url
    | OptionsArg SourceOptions
    | CamIndexArg Int
    | RenderTargetArg RenderTarget
    | UpdateFnArg
    | SideArg
    | GlslFnArg
    | ValuesArg Values
    | AudioArg
    | AudioBinsArg Int
    | WidthHeightArg Int
    | OutputArg OutputN
    | EaseArg EaseType
    | SourceArg SourceN


instance Mark FnArg where
    mark = case _ of
        TArg texture -> mark texture
        VArg value -> mark value
        UrlArg url -> mark url
        OptionsArg options -> mark options
        CamIndexArg _ -> Color.rgb 30 30 205
        RenderTargetArg rt -> mark rt
        ValuesArg values -> mark values
        UpdateFnArg -> Color.rgb 219 112 147
        SideArg -> Color.rgb 155 205 155
        GlslFnArg -> Color.rgb 139 137 137
        AudioArg -> Color.rgb 173 255 47
        AudioBinsArg _ -> Color.rgb 238 230 133
        WidthHeightArg _ -> mark Width
        OutputArg output -> mark output
        EaseArg ease -> mark $ Ease ease
        SourceArg source -> mark source



data FnOut
    = OutTexture
    | OutValues -- same as OutValue?
    | OutValue
    | OutEase
    -- | OutNothing


instance Mark FnOut where
    mark = case _ of
        OutTexture -> mark Empty
        OutValues -> mark $ Values []
        OutValue -> mark None
        OutEase -> mark $ Ease Linear



narg :: Number -> FnArg
narg = VArg <<< Number


tArg :: FnArg
tArg = TArg Empty


urlArg :: FnArg
urlArg = UrlArg $ Url ""


ciArg :: FnArg
ciArg = CamIndexArg 0


optionsArg :: FnArg
optionsArg = OptionsArg $ SourceOptions { src : Canvas }


rtArg :: FnArg
rtArg = RenderTargetArg Four


updateArg :: FnArg
updateArg = UpdateFnArg


sideArg :: FnArg
sideArg = SideArg


glslArg :: FnArg
glslArg = GlslFnArg


valuesArg :: FnArg
valuesArg = ValuesArg $ Values []


audioArg :: FnArg
audioArg = AudioArg


audioBinsArg :: FnArg
audioBinsArg = AudioBinsArg 4


outputArg :: FnArg
outputArg = OutputArg Output0


easeArg :: FnArg
easeArg = EaseArg Linear


sourceArg :: FnArg
sourceArg = SourceArg Source0


tOut :: FnOut
tOut = OutTexture


arrOut :: FnOut
arrOut = OutValues


vOut :: FnOut
vOut = OutValue


eOut :: FnOut
eOut = OutEase


class DefaultOf a where
    default :: a


newtype HydraApiFunctionId = HydraApiFunctionId String


derive instance Newtype HydraApiFunctionId _


hydraAlias_ = "HYDRA" :: String
hydraPrefix_ = hydraAlias_ <> "." :: String


hydraCtor_ :: forall a. Partial => String -> CST.Expr a
hydraCtor_ ctor = exprCtor $ hydraPrefix_ <> ctor


hydraType_ :: forall a. Partial => String -> CST.Type a
hydraType_ ctor = typeCtor $ hydraPrefix_ <> ctor


data HydraApiArguments v
    = Zero
    | One v
    | OneN String v
    | Rec (Array (String /\ v))


-- FIMXE: generate from NDF


class HydraApiFunction a where
    constructor :: a -> String
    hydraFunction :: a -> String /\ HydraApiArguments Value -- always returns Texture



class HydraApiMethod (over :: Type) arg a | a -> over arg where
    mConstructor :: a -> String
    hydraMethod :: a -> String /\ HydraApiArguments arg



functionToPureScript :: forall a. HydraApiFunction a => a -> String
functionToPureScript = signatureHydraFn >>> Sig.toPureScript -- FIXME: implement automatic `ToCode` instance


methodToPureScript :: forall over arg a. ToCode PS Unit arg => HydraApiMethod over arg a => a -> String
methodToPureScript = methodToPureScript' unit -- FIXME: implement automatic `ToCode` instance


methodToPureScript' :: forall opts over arg a. ToCode PS opts arg => opts -> HydraApiMethod over arg a => a -> String
methodToPureScript' opts = signatureHydraMethod >>> toPureScript' opts -- FIXME: implement automatic `ToCode` instance


functionToJavaScript :: forall a. HydraApiFunction a => a -> String
functionToJavaScript = signatureHydraFn >>> Sig.filter argFilter outFilter >>> Sig.toJavaScript -- FIXME: implement automatic `ToCode` instance
    where
        argFilter = Sig.argValue >>> valueIsNone >>> not
        outFilter = const true


methodToJavaScript :: forall over arg a. ToCode JS Unit arg => HydraApiMethod over arg a => a -> String
methodToJavaScript = methodToJavaScript' unit -- FIXME: implement automatic `ToCode` instance


methodToJavaScript' :: forall opts over arg a. ToCode JS opts arg => opts -> HydraApiMethod over arg a => a -> String
methodToJavaScript' opts = signatureHydraMethod >>> toJavaScript' opts -- FIXME: implement automatic `ToCode` instance


-- FIXME: use some special class / instance?
texFunctionToJavaScript :: forall a. HydraApiFunction a => Texture -> a -> String
texFunctionToJavaScript tex tofn =
    case signatureHydraFn tofn of
        Sig (name /\ args /\ outs) -> Sig.toJavaScript $ Sig $ name /\ (Sig.arg "texture" (T tex) : (V <$$> args)) /\ outs


-- FIXME: use some special class / instance?
texFunctionToPureScript :: forall a. HydraApiFunction a => Texture -> a -> String
texFunctionToPureScript tex tofn =
    case signatureHydraFn tofn of
        Sig (name /\ args /\ outs) -> Sig.toPureScript $ Sig $ name /\ (Sig.arg "texture" (T tex) : (V <$$> args)) /\ outs



-- \{ [\w\s,]+\}


instance HydraApiFunction From where
    constructor = case _ of
        Gradient _ -> "Gradient"
        Noise _ -> "Noise"
        Osc _ -> "Osc"
        Shape _ -> "Shape"
        Solid _ -> "Solid"
        Voronoi _ -> "Voronoi"
    hydraFunction = case _ of
        Gradient { speed } -> "gradient" /\ Rec [ "speed" /\ speed ]
        Noise { scale, offset } -> "noise" /\ Rec [ "scale" /\ scale, "offset" /\ offset ]
        Osc { frequency, sync, offset } -> "osc" /\ Rec [ "frequency" /\ frequency, "sync" /\ sync, "offset" /\ offset ]
        Shape { sides, radius, smoothing } -> "shape" /\ Rec [ "sides" /\ sides, "radius" /\ radius, "smoothing" /\ smoothing ]
        Solid { r, g, b, a } -> "solid" /\ Rec [ "r" /\ r, "g" /\ g, "b" /\ b, "a" /\ a ]
        Voronoi { scale, speed, blending } -> "voronoi" /\ Rec [ "scale" /\ scale, "speed" /\ speed, "blending" /\ blending ]


instance HydraApiFunction Geometry where
    constructor = case _ of
        GKaleid _ -> "GKaleid"
        GPixelate _ -> "GPixelate"
        GRepeat _ -> "GRepeat"
        GRepeatX _ -> "GRepeatX"
        GRepeatY _ -> "GRepeatY"
        GRotate _ -> "GRotate"
        GScale _ -> "GScale"
        GScroll _ -> "GScroll"
        GScrollX _ -> "GScrollX"
        GScrollY _ -> "GScrollY"
    hydraFunction = case _ of
        GKaleid { nSides } -> "kaleid" /\ Rec [ "nSides" /\ nSides ]
        GPixelate { pixelX, pixelY } -> "pixelate" /\ Rec [ "pixelX" /\ pixelX, "pixelY" /\ pixelY ]
        GRepeat { repeatX, repeatY, offsetX, offsetY } ->
            "repeat" /\ Rec [ "repeatX" /\ repeatX, "repeatY" /\ repeatY, "offsetX" /\ offsetX, "offsetY" /\ offsetY ]
        GRepeatX { reps, offset } -> "repeatX" /\ Rec [ "reps" /\ reps, "offset" /\ offset ]
        GRepeatY { reps, offset } -> "repeatY" /\ Rec [ "reps" /\ reps, "offset" /\ offset ]
        GRotate { angle, speed } -> "rotate" /\ Rec [ "angle" /\ angle, "speed" /\ speed ]
        GScale { amount, xMult, yMult, offsetX, offsetY } ->
            "scale" /\ Rec [ "amount" /\ amount, "xMult" /\ xMult, "yMult" /\ yMult, "offsetX" /\ offsetX, "offsetY" /\ offsetY ]
        GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ Rec [ "scrollX" /\ scrollX, "scrollY" /\ scrollY, "speedX" /\ speedX, "speedY" /\ speedY ]
        GScrollX { scrollX, speed } -> "scrollX" /\ Rec [ "scrollX" /\ scrollX, "speed" /\ speed ]
        GScrollY { scrollY, speed } -> "scrolly" /\ Rec [ "scrollY" /\ scrollY, "speed" /\ speed ]


instance HydraApiFunction ColorOp where
    constructor = case _ of
        R _ -> "R"
        G _ -> "G"
        B _ -> "B"
        A _ -> "A"
        Posterize _ -> "Posterize"
        Shift _ -> "Shift"
        Color _ -> "Color"
        Luma _ -> "Luma"
        Thresh _ -> "Thresh"
        Invert _ -> "Invert"
        Contrast _ -> "Contrast"
        Saturate _ -> "Saturate"
        Hue _ -> "Hue"
        Colorama _ -> "Colorama"
        Brightness _ -> "Brightness"
    hydraFunction = case _ of
        R { scale, offset } -> "r" /\ Rec [ "scale" /\ scale, "offset" /\ offset ]
        G { scale, offset } -> "g" /\ Rec [ "scale" /\ scale, "offset" /\ offset ]
        B { scale, offset } -> "b" /\ Rec [ "scale" /\ scale, "offset" /\ offset ]
        A { scale, offset } -> "a" /\ Rec [ "scale" /\ scale, "offset" /\ offset ]
        Posterize { bins, gamma } -> "posterize" /\ Rec [ "bins" /\ bins, "gamma" /\ gamma ]
        Shift { r, g, b, a } -> "shift" /\ Rec [ "r" /\ r, "g" /\ g, "b" /\ b, "a" /\ a ]
        Color { r, g, b, a } -> "color" /\ Rec [ "r" /\ r, "g" /\ g, "b" /\ b, "a" /\ a ]
        Luma { threshold, tolerance } -> "luma" /\ Rec [ "threshold" /\ threshold, "tolerance" /\ tolerance ]
        Thresh { threshold, tolerance } -> "thresh" /\ Rec [ "threshold" /\ threshold, "tolerance" /\ tolerance ]
        Invert value -> "invert" /\ One value
        Contrast value -> "contrast" /\ One value
        Saturate value -> "saturate" /\ One value
        Hue value -> "hue" /\ One value
        Colorama value -> "colorama" /\ One value
        Brightness value -> "brightness" /\ One value


instance HydraApiFunction Blend where
    constructor = case _ of
        Blend _ -> "Blend"
        Add _ -> "Add"
        Diff -> "Diff"
        Layer _ -> "Layer"
        Mask -> "Mask"
        Mult _ -> "Mult"
        Sub _ -> "Sub"
    hydraFunction = case _ of
        Blend value -> "blend" /\ One value
        Add value -> "add" /\ One value
        Diff -> "diff" /\ Zero
        Layer value -> "layer" /\ One value
        Mask -> "mask" /\ Zero
        Mult value -> "mult" /\ One value
        Sub value -> "sub" /\ One value


instance HydraApiFunction Modulate where
    constructor = case _ of
        Modulate _ -> "Modulate"
        ModHue _ -> "ModHue"
        ModKaleid _ -> "ModKaleid"
        ModPixelate _ -> "ModPixelate"
        ModRepeat _ -> "ModRepeat"
        ModRepeatX _ -> "ModRepeatX"
        ModRepeatY _ -> "ModRepeatY"
        ModRotate _ -> "ModRotate"
        ModScale _ -> "ModScale"
        ModScroll _ -> "ModScroll"
        ModScrollX _ -> "ModScrollX"
        ModScrollY _ -> "ModScrollY"
    hydraFunction = case _ of
        Modulate value -> "modulate" /\ One value
        ModHue value -> "modulateHue" /\ One value
        ModKaleid { nSides } -> "modulateKaleid" /\ Rec [ "nSides" /\ nSides ]
        ModPixelate { multiple, offset } -> "modulatePixelate" /\ Rec [ "multiple" /\ multiple, "offset" /\ offset ]
        ModRepeat { repeatX, repeatY, offsetX, offsetY } ->
            "modulateRepeat" /\ Rec [ "repeatX" /\ repeatX, "repeatY" /\ repeatY, "offsetX" /\ offsetX, "offsetY" /\ offsetY ]
        ModRepeatX { reps, offset } -> "modulateRepeatX" /\ Rec [ "reps" /\ reps, "offset" /\ offset ]
        ModRepeatY { reps, offset } -> "modulateRepeatY" /\ Rec [ "reps" /\ reps, "offset" /\ offset ]
        ModRotate { multiple, offset } -> "modulateRotate" /\ Rec [ "multiple" /\ multiple, "offset" /\ offset ]
        ModScale { multiple, offset } -> "modulateScale" /\ Rec [ "multiple" /\ multiple, "offset" /\ offset ]
        ModScroll { scrollX, scrollY, speedX, speedY } -> "modulateScroll" /\ Rec [ "scrollX" /\ scrollX, "scrollY" /\ scrollY, "speedX" /\ speedX, "speedY" /\ speedY ]
        ModScrollX { scrollX, speed } -> "modulateScrollX" /\ Rec [ "scrollX" /\ scrollX, "speed" /\ speed ]
        ModScrollY { scrollY, speed } -> "modulateScrollY" /\ Rec [ "scrollY" /\ scrollY, "speed" /\ speed ]


instance HydraApiMethod Values EOrV Ease where
    mConstructor = case _ of
        NoEase -> "NoEase"
        Ease _ -> "Ease"
        Fast _ -> "Fast"
        Smooth _ -> "Smooth"
        Fit _ -> "Fit"
        Offset _ -> "Offset"
    hydraMethod = case _ of
        NoEase -> "" /\ Zero -- FIXME
        Ease arg -> "ease" /\ One (E arg)
        Fast value -> "fast" /\ One (EV value)
        Smooth value -> "smooth" /\ One (EV value)
        Fit { low, high } -> "fit" /\ Rec [ "low" /\ EV low, "high" /\ EV high ]
        Offset value -> "ease" /\ One (EV value)


instance HydraApiMethod AudioSource Value OnAudio where
    mConstructor = case _ of
        Show -> "Show"
        SetBins _ -> "SetBins"
        SetCutoff _ -> "SetCutoff"
        SetScale _ -> "SetScale"
        SetSmooth _ -> "SetSmooth"
        Hide -> "Hide"
    hydraMethod = case _ of
        Show -> "show" /\ Zero
        SetBins n -> "setBins" /\ OneN "numBins" (Number $ Int.toNumber n)
        SetCutoff n -> "setCutoff" /\ OneN "cutoff" (Number n)
        SetScale n -> "setScale" /\ OneN "scale" (Number n)
        SetSmooth n -> "setSmooth" /\ OneN "smooth" (Number n)
        Hide -> "hide" /\ Zero


instance HydraApiMethod Unit FnArg OnSynth where
    mConstructor = case _ of
        Render _ -> "Render"
        Update _ -> "Update"
        SetResolution _ _ -> "SetResolution"
        Hush -> "Hush"
        SetFunction _ -> "SetFunction"
    hydraMethod = case _ of
        Render trg -> "render" /\ One (RenderTargetArg trg)
        Update _ -> "update" /\ One UpdateFnArg
        SetResolution w h -> "setResolution" /\ Rec [ "width" /\ WidthHeightArg w, "height" /\ WidthHeightArg h ]
        Hush -> "hush" /\ Zero
        SetFunction glsl -> "setFunction" /\ One GlslFnArg


instance HydraApiMethod SourceN FnArg OnSource where
    mConstructor = case _ of
        Init _ -> "Init"
        InitCam _ -> "InitCam"
        InitImage _ -> "InitImage"
        InitVideo _ -> "InitImage"
        InitScreen -> "InitScreen"
        Clear -> "Clear"
    hydraMethod = case _ of
        Init sopts -> "init" /\ One (OptionsArg sopts)
        InitCam idx -> "initCam" /\ OneN "index" (CamIndexArg idx)
        InitImage url -> "initCam" /\ OneN "url" (UrlArg url)
        InitVideo url -> "initVideo" /\ OneN "url" (UrlArg url)
        InitScreen -> "initScreen" /\ Zero
        Clear -> "clear" /\ Zero


data HydraApi = HydraApi


instance HydraApiFunction a => ToSignature HydraApi Value out a where
    toSignature :: Proxy HydraApi -> a -> Signature Value out
    toSignature = const signatureHydraFn
else instance HydraApiMethod over arg a => ToSignature HydraApi arg out a where
    toSignature :: Proxy HydraApi -> a -> Signature arg out
    toSignature = const signatureHydraMethod


signatureHydraFn :: forall a out. HydraApiFunction a => a -> Signature Value out -- TODO: out is always `Texture` (?)
signatureHydraFn a = case (hydraFunction a :: String /\ HydraApiArguments Value) of
        ctor /\ Zero       -> Sig.sig' ctor [] []
        ctor /\ One val    -> Sig.sig' ctor [ "amount" /\ val ] []
        ctor /\ OneN name val -> Sig.sig' ctor [ name /\ val ] []
        ctor /\ Rec fields -> Sig.sig' ctor fields []


signatureHydraMethod :: forall over arg out a. HydraApiMethod over arg a => a -> Signature arg out  -- TODO: out is always `Texture` (?)
signatureHydraMethod a = case (hydraMethod a :: String /\ HydraApiArguments arg) of
        ctor /\ Zero       -> Sig.sig' ctor [] []
        ctor /\ One val    -> Sig.sig' ctor [ "amount" /\ val ] []
        ctor /\ OneN name val -> Sig.sig' ctor [ name /\ val ] []
        ctor /\ Rec fields -> Sig.sig' ctor fields []


codegenHydraFn :: forall a. Partial => HydraApiFunction a => a -> CST.Expr Void
codegenHydraFn a =
    case (hydraFunction a :: String /\ HydraApiArguments Value) of
        ctor /\ Zero       -> hydraCtor_ ctor
        ctor /\ One val    -> exprApp (hydraCtor_ ctor) [ mkExpression val ]
        ctor /\ OneN _ val -> exprApp (hydraCtor_ ctor) [ mkExpression val ]
        ctor /\ Rec fields ->
            exprApp
                (hydraCtor_ ctor)
                [ exprRecord (mkExpression <$$> fields)
                ]


codegenHydraMethod :: forall o v a. Partial => ValueCodegen v => HydraApiMethod o v a => a -> CST.Expr Void
codegenHydraMethod a =
    case (hydraMethod a :: String /\ HydraApiArguments v) of
        ctor /\ Zero       -> hydraCtor_ ctor
        ctor /\ One val    -> exprApp (hydraCtor_ ctor) [ mkExpression val ]
        ctor /\ OneN _ val -> exprApp (hydraCtor_ ctor) [ mkExpression val ]
        ctor /\ Rec fields ->
            exprApp
                (hydraCtor_ ctor)
                [ exprRecord (mkExpression <$$> fields)
                ]


instance Partial => ValueCodegen Value where
    mkExpression :: Value -> CST.Expr Void
    mkExpression = case _ of
        None -> hydraCtor_ "None"
        Undefined -> hydraCtor_ "Undefined"
        Number num -> exprApp (hydraCtor_ "Number") [ exprNumber num ]
        VArray (Values vals) ease ->
            exprApp
                (hydraCtor_ "VArray")
                [ exprApp (hydraCtor_ "Values") [ exprArray $ mkExpression <$> vals ]
                , mkExpression ease
                ]
        Dep depFn ->
            exprApp (hydraCtor_ "Dep") [ mkExpression depFn ]
        Time -> hydraCtor_ "Time"
        MouseX -> hydraCtor_ "MouseX"
        MouseY -> hydraCtor_ "MouseY"
        Width -> hydraCtor_ "Width"
        Height -> hydraCtor_ "Height"
        Pi -> hydraCtor_ "Pi"
        Fft audioBin -> exprApp (hydraCtor_ "Fft") [ mkExpression audioBin ]


instance Partial => ValueCodegen TOrV where
    mkExpression :: TOrV -> CST.Expr Void
    mkExpression = case _ of
        T tex -> exprApp (hydraCtor_ "T") [ mkExpression tex ]
        V val -> exprApp (hydraCtor_ "V") [ mkExpression val ]


instance Partial => ValueCodegen EaseType where
    mkExpression :: EaseType -> CST.Expr Void
    mkExpression = hydraCtor_ <<< case _ of
        Linear -> "Linear"
        InOutCubic -> "InOutCubic"


instance Partial => ValueCodegen EOrV where
    mkExpression :: EOrV -> CST.Expr Void
    mkExpression = case _ of
        E ease -> exprApp (hydraCtor_ "E") [ mkExpression ease ]
        EV val -> exprApp (hydraCtor_ "EV") [ mkExpression val ]


instance Partial => ValueCodegen Ease where
    mkExpression :: Ease -> CST.Expr Void
    mkExpression = case _ of
        Ease easeType ->
            exprApp (hydraCtor_ "Ease") [ mkExpression easeType ]
        NoEase ->
            hydraCtor_ "NoEase"
        Fast val ->
            exprApp (hydraCtor_ "Fast") [ mkExpression val ]
        Smooth val ->
            exprApp (hydraCtor_ "Smooth") [ mkExpression val ]
        Fit { low, high } ->
            exprApp (hydraCtor_ "Fit")
                [ exprRecord
                    [ "low" /\ mkExpression low
                    , "high" /\ mkExpression high
                    ]
                ]
        Offset val ->
            exprApp (hydraCtor_ "Offset") [ mkExpression val ]


instance Partial => ValueCodegen DepFn where
    mkExpression :: DepFn -> CST.Expr Void
    mkExpression = const $ hydraCtor_ "NoAction" -- FIXME: TODO


instance Partial => ValueCodegen AudioBin where
    mkExpression :: AudioBin -> CST.Expr Void
    mkExpression (AudioBin n) =
        exprApp (hydraCtor_ "AudioBin") [ exprInt n ]


instance Partial => ValueCodegen From where
    mkExpression :: From -> CST.Expr Void
    mkExpression = codegenHydraFn


instance Partial => ValueCodegen Blend where
    mkExpression :: Blend -> CST.Expr Void
    mkExpression = codegenHydraFn


instance Partial => ValueCodegen ColorOp where
    mkExpression :: ColorOp -> CST.Expr Void
    mkExpression = codegenHydraFn


instance Partial => ValueCodegen Modulate where
    mkExpression :: Modulate -> CST.Expr Void
    mkExpression = codegenHydraFn


instance Partial => ValueCodegen Geometry where
    mkExpression :: Geometry -> CST.Expr Void
    mkExpression = codegenHydraFn


instance Partial => ValueCodegen GlslFnRef where
    mkExpression :: GlslFnRef -> CST.Expr Void
    mkExpression = const $ hydraCtor_ "None" -- FIXME: implement


instance Partial => ValueCodegen TODO where
    mkExpression :: TODO -> CST.Expr Void
    mkExpression = const $ hydraCtor_ "TODO"


instance Partial => ValueCodegen Values where
    mkExpression :: Values -> CST.Expr Void
    mkExpression (Values values) =
        exprApp (hydraCtor_ "Values") [ exprArray $ mkExpression <$> values ]


instance Partial => ValueCodegen Source where
    mkExpression :: Source -> CST.Expr Void
    mkExpression = case _ of
        From from ->
            exprApp (hydraCtor_ "From") [ mkExpression from ]
        Load outputN ->
            exprApp (hydraCtor_ "Load") [ mkExpression outputN ]
        External extSource ->
            exprApp (hydraCtor_ "External") [ mkExpression extSource ]


instance Partial => ValueCodegen OutputN where
    mkExpression :: OutputN -> CST.Expr Void
    mkExpression = hydraCtor_ <<< case _ of
        Output0 -> "Output0"
        Output1 -> "Output1"
        Output2 -> "Output2"
        Output3 -> "Output3"
        Output4 -> "Output4"


instance Partial => ValueCodegen SourceN where
    mkExpression :: SourceN -> CST.Expr Void
    mkExpression = hydraCtor_ <<< case _ of
        Source0 -> "Source0"


instance Partial => ValueCodegen AudioSource where
    mkExpression :: AudioSource -> CST.Expr Void
    mkExpression = hydraCtor_ <<< case _ of
        Silence -> "Silence"
        Mic -> "Mic"
        File -> "File"


instance Partial => ValueCodegen ExtSource where
    mkExpression :: ExtSource -> CST.Expr Void
    mkExpression = case _ of
        Sketch from ->
            exprApp (hydraCtor_ "Sketch") [ exprString from ]
        Video ->
            hydraCtor_ "Video"
        Camera num ->
            exprApp (hydraCtor_ "Camera") [ exprInt num ]
        Unclear ->
            hydraCtor_ "Unclear"


instance Partial => ValueCodegen GlslFnKind where
    mkExpression :: GlslFnKind -> CST.Expr Void
    mkExpression = case _ of
        FnSrc -> hydraCtor_ "FnSrc"
        FnCoord -> hydraCtor_ "FnCoord"
        FnCombineCoord -> hydraCtor_ "FnCombineCoord"
        FnCombine -> hydraCtor_ "FnCombine"
        FnColor -> hydraCtor_ "FnColor"


instance Partial => ValueCodegen GlslFnCode where
    mkExpression :: GlslFnCode -> CST.Expr Void
    mkExpression = case _ of
        GlslFnCode codeStr ->
            exprApp (hydraCtor_ "GlslFnCode")
                [ exprString codeStr ]


instance Partial => ValueCodegen GlslFn where
    mkExpression :: GlslFn -> CST.Expr Void
    mkExpression = case _ of
        GlslFn { kind, code, fn } ->
            exprApp (hydraCtor_ "GlslFn")
                [ exprRecord
                    [ "kind" /\ mkExpression kind
                    , "code" /\ mkExpression code
                    , "fn" /\ mkExpression fn
                    ]
                ]


instance Partial => ValueCodegen Texture where
    mkExpression :: Texture -> CST.Expr Void
    mkExpression = case _ of
        Empty -> hydraCtor_ "Empty"
        Start source -> exprApp (hydraCtor_ "Start") [ mkExpression source ]
        BlendOf { what, with } blend ->
            exprApp (hydraCtor_ "BlendOf")
            [ exprRecord
                [ "what" /\ mkExpression what, "with" /\ mkExpression with ]
            , mkExpression blend
            ]
        Filter texture colorOp ->
            exprApp (hydraCtor_ "Filter")
            [ mkExpression texture
            , mkExpression colorOp
            ]
        ModulateWith { what, with } modulate ->
            exprApp (hydraCtor_ "Modulate")
            [ exprRecord
                [ "what" /\ mkExpression what, "with" /\ mkExpression with ]
            , mkExpression modulate
            ]
        Geometry texture geometry ->
            exprApp (hydraCtor_ "Geometry")
            [ mkExpression texture
            , mkExpression geometry
            ]
        CallGlslFn { over, mbWith } glslFnRef ->
            exprApp (hydraCtor_ "CallGlslFn")
            [ exprRecord
                [ "over" /\ mkExpression over, "mbWith" /\ mkExpression mbWith ]
            , mkExpression glslFnRef
            ]


instance Partial => ValueCodegen RenderTarget where
    mkExpression :: RenderTarget -> CST.Expr Void
    mkExpression = case _ of
        Four -> hydraCtor_ "Four"
        Output outN -> exprApp (hydraCtor_ "Output") [ mkExpression outN ]


instance Partial => ValueCodegen Url where
    mkExpression :: Url -> CST.Expr Void
    mkExpression = case _ of
        Url str -> exprApp (hydraCtor_ "Url") [ exprString str ]


instance Partial => ValueCodegen Canvas where
    mkExpression :: Canvas -> CST.Expr Void
    mkExpression = case _ of
        Canvas -> hydraCtor_ "Canvas"


instance Partial => ValueCodegen SourceOptions where
    mkExpression :: SourceOptions -> CST.Expr Void
    mkExpression = case _ of
        SourceOptions { src } -> exprApp (hydraCtor_ "SourceOptions") [ exprRecord [ "src" /\ mkExpression src ] ]


instance Partial => ValueCodegen UpdateFn where
    mkExpression :: UpdateFn -> CST.Expr Void
    mkExpression = case _ of
        UpdateFn _ -> exprApp (hydraCtor_ "UpdateFn") [ exprLambda [ binderVar "ctx" ] $ exprApp (exprIdent "pure") [ exprIdent "unit" ] ]


valueToJavaScript :: Value -> String
valueToJavaScript = case _ of
    None -> "false /* null */" -- FIXME: Hydra breaks when `null` is passed as an argument to Hydra function
    Undefined -> "false /* undefined */" -- FIXME: Hydra breaks when `undefined` is passed as an argument to Hydra function
    Number n -> show n
    Time -> "time"
    MouseX -> "mouse.x"
    MouseY -> "mouse.y"
    Width -> "width"
    Height -> "height"
    Pi -> "Math.PI"
    Fft (AudioBin bin) -> "() => a.fft[" <> show bin <> "]" -- FIXME
    VArray values ease ->
        valuesToJavaScript values
        <> case ease of
            NoEase -> ""
            theEase -> "." <> methodToJavaScript theEase
    Dep depFn -> "/* dep-fn */" -- FIXME


valueToPureScript :: Value -> String
valueToPureScript = case _ of
    None -> "H.none"
    Undefined -> "H.undefined_"
    Number n -> "H.number " <> show n
    Time -> "H.time"
    MouseX -> "H.mouseX"
    MouseY -> "H.mouseY"
    Width -> "H.width"
    Height -> "H.height"
    Pi -> "H.pi"
    Fft bin -> "H.fft 0" -- FIXME
    VArray values ease -> "[ {- values array -} ]" -- FIXME
    Dep depFn -> "identity {- dep-fn -}" -- FIXME


outputNToJavaScript :: OutputN -> String
outputNToJavaScript = case _ of
    Output0 -> "o0"
    Output1 -> "o1"
    Output2 -> "o2"
    Output3 -> "o3"
    Output4 -> "o4"


outputNToPureScript :: OutputN -> String
outputNToPureScript = outputNToJavaScript


sourceNToJavaScript :: SourceN -> String
sourceNToJavaScript = case _ of
    Source0 -> "s0"


sourceNToPureScript :: SourceN -> String
sourceNToPureScript = sourceNToJavaScript


hasEmptyTexture :: Texture -> Boolean -- TODO: universal fold over textures
hasEmptyTexture = case _ of
    Empty -> true
    Start _ -> false
    Filter tx _ -> hasEmptyTexture tx
    Geometry tx _ -> hasEmptyTexture tx
    BlendOf { what, with } _ ->       hasEmptyTexture what || hasEmptyTexture with
    ModulateWith { what, with } _ ->  hasEmptyTexture what || hasEmptyTexture with
    CallGlslFn { over, mbWith } _ ->  hasEmptyTexture over || case mbWith of
        Just tx -> hasEmptyTexture tx
        Nothing -> false


textureToJavaScript :: Texture -> String
textureToJavaScript = case _ of
    Empty -> "(function() {})"
    Start src ->
        case src of
            From from -> functionToJavaScript from
            Load outputN -> "src( " <> outputNToJavaScript outputN <> " )"
            External sourceN -> "src( " <> sourceNToJavaScript sourceN <> " )"
    Filter texture colorOp ->
        textureToJavaScript texture
        <> "\n\t." <> functionToJavaScript colorOp
    BlendOf { what, with } blendOp ->
        textureToJavaScript what
        <> "\n\t." <> texFunctionToJavaScript with blendOp
    ModulateWith { what, with } modulateOp ->
        textureToJavaScript what
        <> "\n\t." <> texFunctionToJavaScript with modulateOp
    Geometry texture geomOp ->
        textureToJavaScript texture
        <> "\n\t." <> functionToJavaScript geomOp
    CallGlslFn _ _ -> "/* glsl */"


textureToPureScript :: Texture -> String
textureToPureScript = case _ of
    Empty -> "(function() {})"
    Start src -> "/* start */"
    Filter texture colorOp -> textureToPureScript texture <> "." <> functionToPureScript colorOp
    BlendOf { what, with } blendOp -> textureToPureScript what <> "." <> functionToPureScript blendOp
    ModulateWith { what, with } modulateOp -> textureToPureScript what <> "." <> functionToPureScript modulateOp
    Geometry texture geomOp -> textureToPureScript texture <> "." <> functionToPureScript geomOp
    CallGlslFn _ _ -> "/* glsl */"


valueIsNone :: Value -> Boolean
valueIsNone = case _ of
    None -> true
    Undefined -> true
    _ -> false


_valuesArrayToJsCode :: Array Value -> String
_valuesArrayToJsCode = foldl foldVal "" <<< mapWithIndex ((/\))
    where
        foldVal str = case _ of
            (_ /\ None) -> str <> "/* , null */"
            (_ /\ Undefined) -> str <> "/* , undefined */"
            (0 /\ value) -> str <> valueToJavaScript value
            (_ /\ value) -> str <> ", " <> valueToJavaScript value


valuesToJavaScript :: Values -> String
valuesToJavaScript (Values values) =
    "[ " <> _valuesArrayToJsCode values <> " ]"


renderTargetToJavaScript :: RenderTarget -> String
renderTargetToJavaScript renderTrg =
    case renderTrg of
        Four -> "/* FOUR */"
        Output outputN -> outputNToJavaScript outputN


fnArgToJavaScript :: FnArg -> String
fnArgToJavaScript = case _ of
    TArg texture -> textureToJavaScript texture
    VArg val -> valueToJavaScript val
    UrlArg (Url str) -> show str
    OptionsArg opts -> "{ src : canvas }" -- FIXME
    CamIndexArg n -> show n
    ValuesArg values -> valuesToJavaScript values
    AudioArg -> "/* AUDIO */"
    UpdateFnArg -> "/* UPDATE-FN */"
    SideArg -> "/* SIDE */"
    GlslFnArg -> "/* GLSL-FN */"
    EaseArg ease -> easeToJavaScript ease
    OutputArg outputN -> outputNToJavaScript outputN
    RenderTargetArg rTarget -> renderTargetToJavaScript rTarget
    WidthHeightArg whval -> show whval
    AudioBinsArg n -> show n
    SourceArg sourceN -> sourceNToJavaScript sourceN


easeToJavaScript :: EaseType -> String
easeToJavaScript = case _ of
    Linear -> "'linear'"
    InOutCubic -> "'inOutCubic'"


easeToPureScript :: EaseType -> String
easeToPureScript = case _ of
    Linear -> "\"linear\""
    InOutCubic -> "\"inOutCubic\""


instance ToCode JS opts Value where
    toCode = const $ const $ valueToJavaScript


instance ToCode PS opts Value where
    toCode = const $ const $ valueToPureScript


instance ToCode JS opts Texture where
    toCode = const $ const $ textureToJavaScript


instance ToCode PS opts Texture where
    toCode = const $ const $ textureToPureScript


instance ToCode JS opts OutputN where
    toCode = const $ const $ outputNToJavaScript


instance ToCode PS opts OutputN where
    toCode = const $ const $ outputNToPureScript


instance ToCode JS opts TOrV where
    toCode = const $ const $ case _ of
        T tex -> textureToJavaScript tex
        V val -> valueToJavaScript val


instance ToCode PS opts TOrV where
    toCode = const $ const $ case _ of
        T tex -> textureToPureScript tex
        V val -> valueToPureScript val


instance ToCode JS opts EOrV where
    toCode = const $ const $ case _ of
        E ease -> easeToJavaScript ease
        EV val -> valueToJavaScript val


instance ToCode PS opts EOrV where
    toCode = const $ const $ case _ of
        E ease -> easeToPureScript ease
        EV val -> valueToPureScript val


instance ToCode JS opts FnArg where
    toCode = const $ const $ fnArgToJavaScript