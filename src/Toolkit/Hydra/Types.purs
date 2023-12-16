module Tookit.Hydra.Types where

import Prelude

import Effect (Effect)

import Color (Color)
import Color (rgb, black) as Color

import Data.Array ((:))
import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Mark (class Mark, mark)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.Extra as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.FromToFile (class Encode, encode, class Decode, decode)
import Data.Monoid (mempty)

import Type.Proxy (Proxy(..))

import Cli.Palette.Set.X11 as X11

import Noodle.Node.HoldsNodeState (class IsNodeState)

import Tookit.Hydra.Lang.Fn (Fn, FnU) as Lang
import Tookit.Hydra.Lang.Fn (class PossiblyToFn, class ToFn, arg, out, possiblyToFn, q, o, toFn)
import Tookit.Hydra.Lang.Fn (empty, Argument(..), Output(..), name, argName, argValue, argsCount, nameOf, KnownFn) as Fn


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
    | Dep Fn
    | Time
    | MouseX
    | MouseY
    | Width
    | Height
    | Pi
    | Fft AudioBin -- add AudioSource back ?
    -- TODO: Glsl


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


data Fn
    = UserExpr JsExpr
    | Fn (Context -> Effect Value)
    | Unparsed String
    | NoAction


instance Eq Fn where
    eq :: Fn -> Fn -> Boolean
    eq (UserExpr jsExprA) (UserExpr jsExprB) = jsExprA == jsExprB
    eq (Fn _) (Fn _) = false -- could be fixed?
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


data Source
    = Gradient { speed :: Value }
    | Noise { scale :: Value, offset :: Value }
    | Osc { frequency :: Value, sync :: Value, offset :: Value }
    | Shape { sides :: Value, radius :: Value, smoothing :: Value }
    | Solid { r :: Value, g :: Value, b :: Value, a :: Value }
    | Voronoi { scale :: Value, speed :: Value, blending :: Value }
    | Load OutputN
    | External SourceN ExtSource
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


data OutputN
    = Output0
    | Output1
    | Output2
    | Output3
    | Output4 -- name them `OutputN` ?
    -- | ...


data Ease
    = Linear
    | InOutCubic
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


newtype GlslFn = GlslFn (GlslFnKind /\ GlslFnCode /\ Lang.Fn GlslFnArg GlslFnOut) -- holds default value in every argument


newtype GlslFnRef = GlslFnRef (Lang.Fn GlslFnArg GlslFnOut) -- should the name of the function from the registry


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
    = Show AudioSource
    | SetBins AudioSource Int
    | SetCutoff AudioSource Number
    | SetScale AudioSource Number
    | SetSmooth AudioSource Number
    | Hide AudioSource


noUrl :: Url
noUrl = Url ""


defaultUpdateFn :: UpdateFn
defaultUpdateFn = UpdateFn $ const $ pure unit


defaultFn :: Fn
defaultFn = NoAction


defaultTOrV :: TOrV
defaultTOrV = T Empty


defaultGlslFn :: GlslFn
defaultGlslFn = GlslFn $ FnSrc /\ GlslFnCode "" /\ Fn.empty ""


defaultGlslFnRef :: GlslFnRef
defaultGlslFnRef = GlslFnRef $ Fn.empty ""


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


{- MARK -}


instance Mark Value where
    mark :: Value -> Color
    mark = const $ Color.rgb 155 205 155


instance Mark Texture where
    mark :: Texture -> Color
    mark = const $ fromMaybe Color.black X11.darkorange.color
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


instance Mark Fn where
    mark :: Fn -> Color
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

instance Show TOrV where
    show = case _ of
        T tex -> show tex
        V val -> show val


instance Show OTOrV where
    show = case _ of
        OT -> "TEX"
        OV -> "VAL"


instance Show JsExpr where
    show :: JsExpr -> String
    show = case _ of
        Val value -> show value
        AddE v1 v2 -> show v1 <> " + " <> show v2
        SubE v1 v2 -> show v1 <> " - " <> show v2
        MulE v1 v2 -> show v1 <> " * " <> show v2
        DivE v1 v2 -> show v1 <> " / " <> show v2
        ModE v1 v2 -> show v1 <> " % " <> show v2
        Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> show expr <> ")"
                    Nothing -> ""
                )
        Brackets expr -> "( " <> show expr <> " )"


instance Show Fn where
    show :: Fn -> String
    show = case _ of
        UserExpr jsexpr -> ":: " <> show jsexpr <> " ::"
        Fn _ -> "[Code]"
        Unparsed str -> "{{ " <> str <> " }}"
        NoAction -> "--"


instance Show Value where
    show :: Value -> String
    show = case _ of
        None -> "<None>"
        Undefined -> "<Undefined>"
        Number n -> "#" <> show n
        VArray vals ease -> "<" <> show vals <> " at " <> show ease <> ">"
        Dep fn -> "<Dep " <> show fn <> ">"
        Time -> "<Time>"
        MouseX -> "<Mouse X>"
        MouseY -> "<Mouse Y>"
        Width -> "<Width>"
        Height -> "<Height>"
        Pi -> "<Pi>"
        Fft bin -> "<@ " <> show bin <> ">"


instance Show Texture where
    show :: Texture -> String
    show = case _ of
        Empty -> "?"
        Start src -> "• " <> show src
        BlendOf { what, with } blend -> show with <> " + " <> show what <> " >~  ░ " <> show blend
        Filter texture op -> show texture <> " >~ ƒ " <> show op
        ModulateWith { what, with } mod -> show with <> " + " <> show what <> " >~ ¤ " <> show mod
        Geometry texture gmt -> show texture <> " >~ ■ " <> show gmt
        CallGlslFn texture glslFn -> show texture <> " >~ $ " <> show glslFn


        {-
        BlendOf { what, with } blend -> show with <> " + " <> show what <> " >~ " <> show blend
        Filter texture op -> show texture <> " >~ " <> show op
        ModulateWith { what, with } mod -> show with <> " + " <> show what <> " >~ " <> show mod
        Geometry texture gmt -> show texture <> " >~ " <> show gmt
        -}
        {-
        BlendOf { what, with } blend -> show with <> " + " <> show what <> " >~ Blend " <> show blend
        Filter texture op -> show texture <> " >~ Filter " <> show op
        ModulateWith { what, with } mod -> show with <> " + " <> show what <> " >~ Modulate " <> show mod
        Geometry texture gmt -> show texture <> " >~ Geom " <> show gmt
        -}


instance Show Blend where
    show :: Blend -> String
    show = showUsingFnV


instance Show ColorOp where
    show :: ColorOp -> String
    show = showUsingFnV


instance Show Modulate where
    show :: Modulate -> String
    show = showUsingFnV


instance Show Geometry where
    show :: Geometry -> String
    show = showUsingFnV


instance Show TODO where
    show :: TODO -> String
    show = const "TODO"


instance Show Context where
    show :: Context -> String
    show (Context { time }) = "Context { " <> show time <> " }"


instance Show UpdateFn where
    show :: UpdateFn -> String
    show = const "Update Function" -- TODO


instance Show Source where
    show :: Source -> String
    show = showUsingPossiblyFnV $ \s -> case s of
        Gradient { speed } -> "Gradient " <> show speed
        Noise { scale, offset } -> "Noise " <> show scale <> " " <> show offset
        Osc { frequency, sync, offset } -> "Osc " <> show frequency <> " " <> show sync <> " " <> show offset
        Shape { sides, radius, smoothing } -> "Shape " <> show sides <> " " <> show radius <> " " <> show smoothing
        Solid { r, g, b, a } -> "Solid " <> show r <> " " <> show g <> " " <> show b <> " " <> show a
        Voronoi { scale, speed, blending } -> "Voronoi " <> show scale <> " " <> show speed <> " " <> show blending
        Load outputN -> "Load " <> show outputN
        External sourceN ext -> "External " <> show sourceN <> " " <> show ext


instance Show Url where
    show :: Url -> String
    show (Url url) = "Url: " <> show url


instance Show GlslFnKind where
    show :: GlslFnKind -> String
    show = case _ of
        FnSrc -> "Source"
        FnCoord -> "Coord"
        FnCombineCoord -> "CombineCoord"
        FnCombine -> "Combine"
        FnColor -> "Color"


instance Show GlslFn where
    show :: GlslFn -> String
    show (GlslFn (kind /\ _ /\ fn)) = "Define {" <> show kind <> "} " <> showUsingFnTOrV' fn


instance Show GlslFnRef where
    show :: GlslFnRef -> String
    show (GlslFnRef fn) = "Call " <> showUsingFnTOrV' fn


instance Show SourceOptions where
    show :: SourceOptions -> String
    show (SourceOptions { src }) = "Source Options { " {- TODO : <> show src -} <> " }"


instance Show Values where
    show :: Values -> String
    show (Values array) = "[" <> String.joinWith "," (show <$> array) <> "]"


instance Show Ease where
    show :: Ease -> String
    show = case _ of
        Linear -> "Linear"
        Fast v -> "Fast " <> show v
        Smooth v -> "Smooth " <> show v
        Fit { low, high } -> "Fit " <> show low <> " < " <> show high
        Offset v -> "Offset " <> show v
        InOutCubic -> "InOutCubic"


instance Show AudioSource where
    show :: AudioSource -> String
    show = case _ of
        Silence -> "Silence"
        Mic -> "Microphone"
        File -> "File"


instance Show AudioBin where
    show :: AudioBin -> String
    show (AudioBin n) = "@" <> show n


instance Show SourceN where
    show :: SourceN -> String
    show = case _ of
        Source0 -> "Source 0"


instance Show ExtSource where
    show :: ExtSource -> String
    show = case _ of
        Camera n -> "Camera " <> show n -- 🎥
        Sketch name -> "Sketch " <> name
        Video -> "Video"
        Unclear -> "Unclear"


instance Show CanBeSource where
    show :: CanBeSource -> String
    show (CanBeSource cbs) = case cbs of
        Left sourceN -> show sourceN
        Right outputN -> show outputN


instance Show OutputN where
    show :: OutputN -> String
    show = case _ of
        Output0 -> "Output 0"
        Output1 -> "Output 1"
        Output2 -> "Output 2"
        Output3 -> "Output 3"
        Output4 -> "Output 4"


instance Show RenderTarget where
    show :: RenderTarget -> String
    show Four = "Four"
    show (Output oN) = show oN


instance Encode Value where
    encode :: Value -> String
    encode = case _ of
        None -> "0 V"
        Undefined -> "U V"
        Number n -> "N " <> encode n
        VArray vals ease -> "VA " <> encode vals <> " $$ " <> encode ease <> ""
        Dep fn -> "D " <> encode fn
        Time -> "T V"
        MouseX -> "MX V"
        MouseY -> "MY V"
        Width -> "W V"
        Height -> "H V"
        Pi -> "PI V"
        Fft bin -> "A " <> encode bin


instance Encode Texture where
    encode :: Texture -> String
    encode = case _ of
        Empty -> "EMP T"
        Start src -> "S " <> encode src
        BlendOf { what, with } blend -> "B " <> encode what <> texSep <> encode with <> texSep <> encode blend <> texsEnd
        Filter texture op -> "F " <> encode op <> texSep <> encode texture <> texsEnd
        ModulateWith { what, with } mod -> "M " <> encode what <> texSep <> encode with <> texSep <> encode mod <> texsEnd
        Geometry texture gmt -> "G " <> encode texture <> texSep <> encode gmt <> texsEnd
        CallGlslFn { over, mbWith } fn ->
            "CALL " <> encode over <> texSep
                <> (
                    case mbWith of
                        Just with -> encode with <> texSep
                        Nothing -> mempty
                )
                <> encode fn <> texsEnd


instance Encode Blend where
    encode :: Blend -> String
    encode = encodeUsingFn


instance Encode ColorOp where
    encode :: ColorOp -> String
    encode = encodeUsingFn


instance Encode Modulate where
    encode :: Modulate -> String
    encode = encodeUsingFn


instance Encode Geometry where
    encode :: Geometry -> String
    encode = encodeUsingFn


instance Encode TODO where
    encode :: TODO -> String
    encode = const "TODO"


instance Encode Context where
    encode :: Context -> String
    encode (Context { time }) = "{ " <> encode time <> " }"


instance Encode UpdateFn where
    encode :: UpdateFn -> String
    encode = const "UF" -- TODO


instance Encode ExtSource where
    encode :: ExtSource -> String
    encode = case _ of
        Camera n -> "C " <> show n
        Sketch name -> "SK " <> name
        Video -> "V X"
        Unclear -> "U X"


instance Encode Source where
    encode :: Source -> String
    encode = case _ of
        Load outputN -> "O " <> encode outputN
        External sourceN def -> "X " <> encode sourceN <> argSep <> encode def <> argsEnd
        Gradient { speed } -> "G " <> encode speed <> argsEnd
        Noise { scale, offset } -> "N " <> encode scale <> argSep <> encode offset <> argsEnd
        Osc { frequency, sync, offset } -> "OSC " <> encode frequency <> argSep <> encode sync <> argSep <> encode offset <> argsEnd
        Shape { sides, radius, smoothing } -> "SHP " <> encode sides <> argSep <> encode radius <> argSep <> encode smoothing <> argsEnd
        Solid { r, g, b, a } -> "S " <> encode r <> argSep <> encode g <> argSep <> encode b <> argSep <> encode a <> argsEnd
        Voronoi { scale, speed, blending } -> "V " <> encode scale <> argSep <> encode speed <> argSep <> encode blending <> argsEnd


instance Encode RenderTarget where
    encode :: RenderTarget -> String
    encode Four = "ALL"
    encode (Output on) = show on


instance Encode Url where
    encode :: Url -> String
    encode (Url url) = encode url


instance Encode GlslFnKind where
    encode :: GlslFnKind -> String
    encode = case _ of
        FnSrc -> "SRC"
        FnCoord -> "CRD"
        FnCombineCoord -> "CCR"
        FnCombine -> "CMB"
        FnColor -> "CLR"


instance Encode TOrV where
    encode :: TOrV -> String
    encode = case _ of
        T tex -> "TT " <> encode tex
        V val -> "VV " <> encode val


instance Encode GlslFn where
    encode :: GlslFn -> String
    encode (GlslFn (kind /\ GlslFnCode code /\ fn))
        = encode kind <> " "
            <> glslStart <> encode code <> glslEnd
            <> " " <> encodeFnWithArgNames fn


instance Encode GlslFnRef where
    encode :: GlslFnRef -> String
    encode (GlslFnRef fn)
        = encodeFnWithArgNames fn


{- instance Encode (Lang.Fn TOrV) where
    encode :: Lang.Fn TOrV -> String
    encode _ = "" -}


instance Encode CanBeSource where
    encode :: CanBeSource -> String
    encode (CanBeSource cbs) =
        case cbs of
            Left sourceN -> "L " <> encode sourceN
            Right outputN -> "R " <> encode outputN


instance Encode SourceOptions where
    encode :: SourceOptions -> String
    encode (SourceOptions { src }) = "SO " -- TODO: <> encode src


instance Encode Values where
    encode :: Values -> String
    encode (Values array) = "%% " <> String.joinWith " <> " (encode <$> array) <> " %%"


instance Encode Ease where
    encode :: Ease -> String
    encode = case _ of
        Linear -> "LIN E"
        Fast v -> "FST " <> encode v
        Smooth v -> "SMT " <> encode v
        Fit { low, high } -> "FIT " <> encode low <> " < " <> encode high
        Offset v -> "OFF " <> encode v
        InOutCubic -> "IOC E"


instance Encode AudioSource where
    encode :: AudioSource -> String
    encode = case _ of
        Silence -> "SIL"
        Mic -> "MIC"
        File -> "FIL"


instance Encode AudioBin where
    encode :: AudioBin -> String
    encode (AudioBin n) = "@" <> show n


instance Encode OutputN where
    encode :: OutputN -> String
    encode = case _ of
        Output0 -> "O0"
        Output1 -> "O1"
        Output2 -> "O2"
        Output3 -> "O3"
        Output4 -> "O4"


instance Encode SourceN where
    encode :: SourceN -> String
    encode = case _ of
        Source0 -> "S0"


instance Encode JsExpr where
    encode :: JsExpr -> String
    encode = case _ of
        Val value -> show value -- FIXME: for sure, not `encode`?
        AddE v1 v2 -> show v1 <> " + " <> show v2 -- FIXME: for sure, not `encode`?
        SubE v1 v2 -> show v1 <> " - " <> show v2 -- FIXME: for sure, not `encode`?
        MulE v1 v2 -> show v1 <> " * " <> show v2 -- FIXME: for sure, not `encode`?
        DivE v1 v2 -> show v1 <> " / " <> show v2 -- FIXME: for sure, not `encode`?
        ModE v1 v2 -> show v1 <> " % " <> show v2 -- FIXME: for sure, not `encode`?
        Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> show expr <> ")"
                    Nothing -> ""
                )
        Brackets expr -> "( " <> show expr <> " )"


instance Encode Fn where
    encode :: Fn -> String
    encode = case _ of
        UserExpr jsexpr -> encode jsexpr
        Fn _ -> "[[CODE]]"
        Unparsed str -> unparsedFnStart <> str <> unparsedFnEnd -- "<<<< " <> str <> " >>>>"
        NoAction -> "/----/"


data TOrV
    = T Texture
    | V Value


data OTOrV
    = OT
    | OV


instance ToFn Value Unit ColorOp where
    toFn :: ColorOp -> String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)
    toFn = case _ of
        R { scale, offset } -> "r" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        G { scale, offset } -> "g" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        B { scale, offset } -> "b" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        A { scale, offset } -> "a" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        Posterize { bins, gamma } -> "posterize" /\ [ q "bins" bins, q "gamma" gamma ] /\ [ o "out" unit ]
        Shift { r, g, b, a } -> "shift" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ] /\ [ o "out" unit ]
        Invert amount -> "invert" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Contrast amount -> "contrast" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Brightness amount -> "brightness" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Luma { threshold, tolerance } -> "luma" /\ [ q "threshold" threshold, q "tolerance" tolerance ] /\ [ o "out" unit ]
        Thresh { threshold, tolerance } -> "thresh" /\ [ q "threshold" threshold, q "tolerance" tolerance ] /\ [ o "out" unit ]
        Color { r, g, b, a } -> "color" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ] /\ [ o "out" unit ]
        Saturate amount -> "saturate" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Hue amount -> "hue" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Colorama amount -> "colorama" /\ [ q "amount" amount ] /\ [ o "out" unit ]


instance ToFn Value Unit Modulate where
    toFn :: Modulate -> String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)
    toFn = case _ of
        Modulate amount -> "modulate" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        ModHue amount -> "modHue" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        ModKaleid { nSides } -> "modKaleid" /\ [ q "nSides" nSides ] /\ [ o "out" unit ]
        ModPixelate { multiple, offset } -> "modPixelate" /\ [ q "multiple" multiple, q "offset" offset ] /\ [ o "out" unit ]
        ModRepeat { repeatX, repeatY, offsetX, offsetY } -> "modRepeat" /\ [ q "repeatX" repeatX, q "repeatY" repeatY, q "offsetX" offsetX, q "offsetY" offsetY ] /\ [ o "out" unit ]
        ModRepeatX { reps, offset } -> "modRepeatX" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        ModRepeatY { reps, offset } -> "modRepeatY" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        ModRotate { multiple, offset } -> "modRotate" /\ [ q "multiple" multiple, q "offset" offset ] /\ [ o "out" unit ]
        ModScale { multiple, offset } -> "modScale" /\ [ q "multiple" multiple, q "offset" offset ] /\ [ o "out" unit ]
        ModScroll { scrollX, scrollY, speedX, speedY } -> "modScroll" /\ [ q "scrollX" scrollX, q "scrollY" scrollY, q "speedX" speedX, q "speedY" speedY ] /\ [ o "out" unit ]
        ModScrollX { scrollX, speed } -> "modScrollX" /\ [ q "scrollX" scrollX, q "speed" speed ] /\ [ o "out" unit ]
        ModScrollY { scrollY, speed } -> "modScrollY" /\ [ q "scrollY" scrollY, q "speed" speed ] /\ [ o "out" unit ]


instance ToFn Value Unit Blend where
    toFn :: Blend -> String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)
    toFn = case _ of
        Blend amount -> "blend" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Add amount -> "add" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Sub amount -> "sub" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Mult amount -> "mult" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Layer amount -> "layer" /\ [ q "amount" amount ] /\ [ o "out" unit ]
        Diff -> "diff" /\ [] /\ []
        Mask -> "mask" /\ [] /\ []


instance ToFn Value Unit Geometry where
    toFn :: Geometry -> String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)
    toFn = case _ of
        GKaleid { nSides } -> "kaleid" /\ [ q "nSides" nSides ] /\ [ o "out" unit ]
        GPixelate { pixelX, pixelY } -> "pixelate" /\ [ q "pixelX" pixelX, q "pixelY" pixelY ] /\ [ o "out" unit ]
        GRepeat { repeatX, repeatY, offsetX, offsetY } -> "repeat" /\ [ q "repeatX" repeatX, q "repeatY" repeatY, q "offsetX" offsetX, q "offsetY" offsetY ] /\ [ o "out" unit ]
        GRepeatX { reps, offset } -> "repeatX" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        GRepeatY { reps, offset } -> "repeatY" /\ [ q "reps" reps, q "offset" offset ] /\ [ o "out" unit ]
        GRotate { angle, speed } -> "rotate" /\ [ q "angle" angle, q "speed" speed ] /\ [ o "out" unit ]
        GScale { amount, xMult, yMult, offsetX, offsetY } -> "scale" /\ [ q "amount" amount, q "xMult" xMult, q "yMult" yMult, q "offsetX" offsetX, q "offsetY" offsetY ] /\ [ o "out" unit ]
        GScroll { scrollX, scrollY, speedX, speedY } -> "scroll" /\ [ q "scrollX" scrollX, q "scrollY" scrollY, q "speedX" speedX, q "speedY" speedY ] /\ [ o "out" unit ]
        GScrollX { scrollX, speed } -> "scrollX" /\ [ q "scrollX" scrollX, q "speed" speed ] /\ [ o "out" unit ]
        GScrollY { scrollY, speed } -> "scrollY" /\ [ q "scrollY" scrollY, q "speed" speed ] /\ [ o "out" unit ]


instance ToFn Value Unit Ease where
    toFn :: Ease -> String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)
    toFn = case _ of
        Linear -> "linear" /\ [] /\ [ o "out" unit ]
        Fast v -> "fast" /\ [ q "v" v ] /\ [ o "out" unit ]
        Smooth v -> "smooth" /\ [ q "v" v ] /\ [ o "out" unit ]
        Fit { low, high } -> "fit" /\ [ q "low" low, q "high" high ] /\ [ o "out" unit ]
        Offset v -> "offset" /\ [ q "v" v ] /\ [ o "out" unit ]
        InOutCubic -> "inOutCubic" /\ [] /\ [ o "out" unit ]


instance ToFn GlslFnArg GlslFnOut GlslFn where
    toFn :: GlslFn -> String /\ Array (Fn.Argument GlslFnArg) /\ Array (Fn.Output GlslFnOut)
    toFn (GlslFn (name /\ _ /\ fn)) = toFn fn


instance ToFn GlslFnArg GlslFnOut GlslFnRef where
    toFn :: GlslFnRef -> String /\ Array (Fn.Argument GlslFnArg) /\ Array (Fn.Output GlslFnOut)
    toFn (GlslFnRef fn) = toFn fn


instance PossiblyToFn Value Unit Source where
    possiblyToFn :: Source -> Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))
    possiblyToFn = case _ of
        Load outputN -> Nothing -- TODO: could be converted to `src()`
        External sourceN ext -> Nothing -- TODO: could be converted to `src()` ?
        Gradient { speed } -> Just $ "gradient" /\ [ q "speed" speed ] /\ [ o "out" unit ]
        Noise { scale, offset } -> Just $ "noise" /\ [ q "scale" scale, q "offset" offset ] /\ [ o "out" unit ]
        Osc { frequency, sync, offset } -> Just $ "osc" /\ [ q "frequency" frequency, q "sync" sync, q "offset" offset ] /\ [ o "out" unit ]
        Shape { sides, radius, smoothing } -> Just $ "shape" /\ [ q "sides" sides, q "radius" radius, q "smoothing" smoothing ] /\ [ o "out" unit ]
        Solid { r, g, b, a } -> Just $ "solid" /\ [ q "r" r, q "g" g, q "b" b, q "a" a ] /\ [ o "out" unit ]
        Voronoi { scale, speed, blending } -> Just $ "voronoi" /\ [ q "scale" scale, q "speed" speed, q "blending" blending ] /\ [ o "out" unit ]


instance PossiblyToFn TOrV OTOrV Texture where
    possiblyToFn :: Texture -> Maybe (String /\ Array (Fn.Argument TOrV) /\ Array (Fn.Output OTOrV))
    possiblyToFn = case _ of
        Empty -> Nothing
        Start src ->
            case (possiblyToFn src :: Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))) of
                Just (name /\ args /\ outs) -> Just $ name /\ (map V <$> args) /\ (map (const OT) <$> outs)
                Nothing -> Nothing
        BlendOf { what, with } blend ->
            case (toFn blend :: String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)) of
                name /\ args /\ outs -> Just $ name /\ ((q "what" $ T what) : (map V <$> args) <> [ q "with" $ T with ]) /\ (map (const OT) <$> outs)
        Filter texture cop ->
            case (toFn cop :: String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)) of
                name /\ args /\ outs -> Just $ name /\ ((map V <$> args) <> [ q "texture" $ T texture ]) /\ (map (const OT) <$> outs)
        ModulateWith { what, with } mod ->
            case (toFn mod :: String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)) of
                name /\ args /\ outs -> Just $ name /\ ((q "what" $ T what) : (map V <$> args) <> [ q "with" $ T with ]) /\ (map (const OT) <$> outs)
        Geometry texture gmt ->
            case (toFn gmt :: String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit)) of
                name /\ args /\ outs -> Just $ name /\ ((map V <$> args) <> [ q "texture" $ T texture ]) /\ (map (const OT) <$> outs)
        CallGlslFn { over, mbWith } fnRef ->
            case (toFn fnRef :: String /\ Array (Fn.Argument TOrV) /\ Array (Fn.Output GlslFnOut)) of
                name /\ args /\ outs -> Just $ name /\ ((q "over" $ T over) : args <>
                    case mbWith of
                        Just with -> [ q "with" $ T with ]
                        Nothing -> [ ]
                    ) /\ (map (const OT) <$> outs)


instance PossiblyToFn Value Unit Fn.KnownFn where
    possiblyToFn :: Fn.KnownFn -> Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))
    possiblyToFn = Fn.nameOf >>> fromKnownFn


encodeFnWithArgNames :: forall arg out. Encode arg => Lang.Fn arg out -> String
encodeFnWithArgNames fn =
    case (toFn fn :: String /\ Array (Fn.Argument arg) /\ Array (Fn.Output out)) of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                name <> " " <> show (Array.length args) <> " " <> String.joinWith argSep (encodeArg <$> args) <> argsEnd
            else
                name <> " " <> show (Array.length args) <> " " <> argsEnd
    where
        encodeArg arg =
            Fn.argName arg <> "::" <> encode (Fn.argValue arg)
    -- Fn.name fn


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
    | OutputArg OutputN
    | EaseArg Ease
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
        OutputArg output -> mark output
        EaseArg ease -> mark ease
        SourceArg source -> mark source



data FnOut
    = OutTexture
    | OutValues -- same as OutValue?
    | OutValue
    | OutEase
    -- | OutNothing


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


instance PossiblyToFn FnArg FnOut Fn.KnownFn where
    possiblyToFn = Fn.nameOf >>> defaultsFor


defaultsFor :: String -> Maybe (String /\ Array (Fn.Argument FnArg) /\ Array (Fn.Output FnOut)) -- TODO: output of Fn!
-- defaultValuesOf :: FamilyR -> Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))
defaultsFor = case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ q "scale" $ narg 10.0, q "offset" $ narg 0.1 ] /\ [ o "out" tOut ]
    "voronoi" -> Just $ "voronoi" /\ [ q "scale" $ narg 5.0, q "speed" $ narg 0.3, q "blending" $ narg 0.3 ] /\ [ o "out" tOut ]
    "osc" -> Just $ "osc" /\ [ q "frequency" $ narg 60.0, q "sync" $ narg 0.1, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "shape" -> Just $ "shape" /\ [ q "sides" $ narg 3.0, q "radius" $ narg 0.3, q "smoothing" $ narg 0.01 ] /\ [ o "out" tOut ]
    "gradient" -> Just $ "gradient" /\ [ q "speed" $ narg 0.0 ] /\ [ o "out" tOut ]
    "src" -> Just $ "src" /\ [ q "src" sourceArg  ] /\ [ o "out" tOut ]
    "solid" -> Just $ "solid" /\ [ q "r" $ narg 0.0, q "g" $ narg 0.0, q "b" $ narg 0.0, q "a" $ narg 1.0 ] /\ [ o "out" tOut ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ q "what" tArg, q "angle" $ narg 10.0, q "speed" $ narg 0.0 ] /\ [ o "out" tOut ]
    "scale" -> Just $ "scale" /\ [ q "what" tArg, q "amount" $ narg 1.5, q "xMult" $ narg 1.0, q "yMult" $ narg 1.0, q "offsetX" $ narg 0.5, q "offsetY" $ narg 0.5 ] /\ [ o "out" tOut ]
    "pixelate" -> Just $ "pixelate" /\ [ q "what" tArg, q "pixelX" $ narg 20.0, q "pixelY" $ narg 20.0 ] /\ [ o "out" tOut ]
    "repeat" -> Just $ "repeat" /\ [ q "what" tArg, q "repeatX" $ narg 3.0, q "repeatY" $ narg 3.0, q "offsetX" $ narg 0.0, q "offsetY" $ narg 0.0 ] /\ [ o "out" tOut ]
    "repeatX" -> Just $ "repeatX" /\ [ q "what" tArg, q "reps" $ narg 3.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "repeatY" -> Just $ "repeatY" /\ [ q "what" tArg, q "reps" $ narg 3.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "kaleid" -> Just $ "kaleid" /\ [ q "what" tArg, q "nSides" $ narg 4.0 ] /\ [ o "out" tOut ]
    "scroll" -> Just $ "scroll" /\ [ q "what" tArg, q "scrollX" $ narg 0.5, q "scrollY" $ narg 0.5, q "speedX" $ narg 0.0, q "speedY" $ narg 0.0 ] /\ [ o "out" tOut ]
    "scrollX" -> Just $ "scrollX" /\ [ q "what" tArg, q "scrollX" $ narg 0.5, q "speed" $ narg 0.0 ] /\ [ o "out" tOut ]
    "scrollY" -> Just $ "scrollY" /\ [ q "what" tArg, q "scrollY" $ narg 0.5, q "speed" $ narg 0.0 ] /\ [ o "out" tOut ]

    "posterize" -> Just $ "posterize" /\ [ q "what" tArg, q "bins" $ narg 3.0, q "gamma" $ narg 6.0 ] /\ [ o "out" tOut ]
    "shift" -> Just $ "shift" /\ [ q "what" tArg, q "r" $ narg 0.5, q "g" $ narg 0.0, q "b" $ narg 0.0, q "a" $ narg 0.5 ] /\ [ o "out" tOut ]
    "invert" -> Just $ "invert" /\ [ q "what" tArg, q "amount" $ narg 1.0 ] /\ [ o "out" tOut ]
    "contrast" -> Just $ "contrast" /\ [ q "what" tArg, q "amount" $ narg 1.6 ] /\ [ o "out" tOut ]
    "brightness" -> Just $ "brightness" /\ [ q "what" tArg, q "amount" $ narg 0.4 ] /\ [ o "out" tOut ]
    "luma" -> Just $ "luma" /\ [ q "what" tArg, q "threshold" $ narg 0.5, q "tolerance" $ narg 0.1 ] /\ [ o "out" tOut ]
    "thresh" -> Just $ "thresh" /\ [ q "what" tArg, q "threshold" $ narg 0.5, q "tolerance" $ narg 0.04 ] /\ [ o "out" tOut ]
    "color" -> Just $ "color" /\ [ q "what" tArg, q "r" $ narg 1.0, q "g" $ narg 1.0, q "b" $ narg 1.0, q "a" $ narg 1.0 ] /\ [ o "out" tOut ]
    "saturate" -> Just $ "saturate" /\ [ q "what" tArg, q "amount" $ narg 2.0 ] /\ [ o "out" tOut ]
    "hue" -> Just $ "hue" /\ [ q "what" tArg, q "amount" $ narg 0.4 ] /\ [ o "out" tOut ]
    "colorama" -> Just $ "colorama" /\ [ q "what" tArg, q "amount" $ narg 0.005 ] /\ [ o "out" tOut ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ q "what" tArg, q "scale" $ narg 1.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "g" -> Just $ "g" /\ [ q "what" tArg, q "scale" $ narg 1.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "b" -> Just $ "b" /\ [ q "what" tArg, q "scale" $ narg 1.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "a" -> Just $ "a" /\ [ q "what" tArg, q "scale" $ narg 1.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ q "what" tArg, q "with" tArg, q "amount" $ narg 1.0 ] /\ [ o "out" tOut ]
    "sub" -> Just $ "sub" /\ [ q "what" tArg, q "with" tArg, q "amount" $ narg 1.0 ] /\ [ o "out" tOut ]
    "layer" -> Just $ "layer" /\ [ q "what" tArg, q "with" tArg ] /\ [ o "out" tOut ]
    "blend" -> Just $ "blend" /\ [ q "what" tArg, q "with" tArg, q "amount" $ narg 0.5 ] /\ [ o "out" tOut ]
    "mult" -> Just $ "mult" /\ [ q "what" tArg, q "with" tArg, q "amount" $ narg 1.0 ] /\ [ o "out" tOut ]
    "diff" -> Just $ "diff" /\ [ q "what" tArg, q "with" tArg ] /\ [ o "out" tOut ]
    "mask" -> Just $ "mask" /\ [ q "what" tArg, q "with" tArg ] /\ [ o "out" tOut ]

    "modulateRepeat" -> Just $ "modRepeat" /\ [ q "what" tArg, q "with" tArg, q "repeatX" $ narg 3.0, q "repeatY" $ narg 3.0, q "offsetX" $ narg 0.5, q "offsetY" $ narg 0.5 ] /\ [ o "out" tOut ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ q "what" tArg, q "with" tArg, q "reps" $ narg 3.0, q "offset" $ narg 0.5 ] /\ [ o "out" tOut ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ q "what" tArg, q "with" tArg, q "reps" $ narg 3.0, q "offset" $ narg 0.5 ] /\ [ o "out" tOut ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ q "what" tArg, q "with" tArg, q "nSides" $ narg 4.0 ] /\ [ o "out" tOut ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ q "what" tArg, q "with" tArg, q "scrollX" $ narg 0.5, q "speed" $ narg 0.0 ] /\ [ o "out" tOut ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ q "what" tArg, q "with" tArg, q "scrollY" $ narg 0.5, q "speed" $ narg 0.0 ] /\ [ o "out" tOut ]
    "modulate" -> Just $ "modulate" /\ [ q "what" tArg, q "with" tArg, q "amount" $ narg 0.1 ] /\ [ o "out" tOut ]
    "modulateScale" -> Just $ "modScale" /\ [ q "what" tArg, q "with" tArg, q "multiple" $ narg 1.0, q "offset" $ narg 1.0 ] /\ [ o "out" tOut ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ q "what" tArg, q "with" tArg, q "multiple" $ narg 10.0, q "offset" $ narg 3.0 ] /\ [ o "out" tOut ]
    "modulateRotate" -> Just $ "modRotate" /\ [ q "what" tArg, q "with" tArg, q "multiple" $ narg 1.0, q "offset" $ narg 0.0 ] /\ [ o "out" tOut ]
    "modulateHue" -> Just $ "modHue" /\ [ q "what" tArg, q "with" tArg, q "amount" $ narg 1.0 ] /\ [ o "out" tOut ]

    "initCam" -> Just $ "initCam" /\ [] /\ []
    "initImage" -> Just $ "initImage" /\ [ q "url" urlArg ] /\ []
    "initVideo" -> Just $ "initVideo" /\ [ q "url" urlArg ] /\ []
    "init" -> Just $ "init" /\ [ q "options" optionsArg ] /\ []
    "initStream" -> Just $ "initStream" /\ [ q "url" urlArg ] /\ []
    "initCam" -> Just $ "initCam" /\ [ q "index" ciArg ] /\ []
    "initScreen" -> Just $ "initScreen" /\ [] /\ []

    "render" -> Just $ "render" /\ [ q "target" rtArg ] /\ []
    "update" -> Just $ "update" /\ [ q "update" updateArg ] /\ []
    "setResolution" -> Just $ "setResolution" /\ [ q "width" sideArg, q "height" sideArg ] /\ []
    "hush" -> Just $ "hush" /\ [] /\ []
    "setFunction" -> Just $ "setFunction" /\ [ q "fn" glslArg ] /\ []

    -- "speed" -> Synth
    -- "bpm" -> Synth
    "width" -> Just $ "width" /\ [] /\ [ o "out" vOut ]
    "height" -> Just $ "height" /\ [] /\ [ o "out" vOut ]
    "time" -> Just $ "time" /\ [] /\ [ o "out" vOut ]
    "mouse" -> Just $ "time" /\ [] /\ [ o "x" vOut, o "y" vOut ]
    "pi" -> Just $ "time" /\ [] /\ [ o "out" vOut ]

    "fft" -> Just $ "fft" /\ [ q "audio" audioArg ] /\ [ o "out" vOut ]
    "setSmooth" -> Just $ "setSmooth" /\ [ q "audio" audioArg, q "smooth" $ narg 0.4 ] /\ []
    "setCutoff" -> Just $ "setCutoff" /\ [ q "audio" audioArg, q "cutoff" $ narg 2.0 ] /\ []
    "setBins" -> Just $ "setCutoff" /\ [ q "audio" audioArg, q "bins" audioBinsArg ] /\ []
    "setScale" -> Just $ "setScale" /\ [ q "audio" audioArg, q "scale" $ narg 10.0 ] /\ []
    "hide" -> Just $ "hide" /\ [] /\ []
    "show" -> Just $ "show" /\ [] /\ []

    "out" -> Just $ "out" /\ [ q "output" outputArg ] /\ []

    "linear" -> Just $ "linear" /\ [ q "array" valuesArg ] /\ [ o "out" arrOut ]
    "ease" -> Just $ "ease" /\ [ q "ease" easeArg ] /\ [ o "out" arrOut ]
    "fast" -> Just $ "fast" /\ [ q "array" valuesArg, q "v" $ narg 1.0 ] /\ [ o "out" arrOut ]
    "smooth" -> Just $ "smooth" /\ [ q "array" valuesArg, q "v" $ narg 1.0 ] /\ [ o "out" arrOut ]
    "offset" -> Just $ "offset" /\ [ q "array" valuesArg, q "v" $ narg 0.5 ] /\ [ o "out" arrOut ]
    "fit" -> Just $ "fit" /\ [ q "array" valuesArg, q "low" $ narg 0.0, q "high" $ narg 1.0 ] /\ [ o "out" arrOut ]

    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing



{- instance PossiblyToFn Value Fn Unit.KnownFn where
    possiblyToFn = Fn.nameOf >>> defaultsFor -}

-- instance FnDefault // FnDocs // FnTypes


-- TODO: probably duplicates something, is it used? replace with above instance of `defaultsFor`?`
-- TODO: private
fromKnownFn :: String -> Maybe (String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit))
fromKnownFn = case _ of
    -- "number" -> Feed

    "noise" -> Just $ "noise" /\ [ q "scale" $ Number 10.0, q "offset" $ Number 0.1 ] /\ [ o "out" unit ]
    "voronoi" -> Just $ "voronoi" /\ [ q "scale" $ Number 5.0, q "speed" $ Number 0.3, q "blending" $ Number 0.3 ] /\ [ o "out" unit ]
    "osc" -> Just $ "osc" /\ [ q "frequency" $ Number 60.0, q "sync" $ Number 0.1, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "shape" -> Just $ "shape" /\ [ q "sides" $ Number 3.0, q "radius" $ Number 0.3, q "smoothing" $ Number 0.01 ] /\ [ o "out" unit ]
    "gradient" -> Just $ "gradient" /\ [ q "speed" $ Number 0.0 ] /\ [ o "out" unit ]
    -- "src" -> Source
    "solid" -> Just $ "solid" /\ [ q "r" $ Number 0.0, q "g" $ Number 0.0, q "b" $ Number 0.0, q "a" $ Number 1.0 ] /\ [ o "out" unit ]
    -- "prev" -> Source

    "rotate" -> Just $ "rotate" /\ [ q "angle" $ Number 10.0, q "speed" $ Number 0.0 ] /\ [ o "out" unit ]
    "scale" -> Just $ "scale" /\ [ q "amount" $ Number 1.5, q "xMult" $ Number 1.0, q "yMult" $ Number 1.0, q "offsetX" $ Number 0.5, q "offsetY" $ Number 0.5 ] /\ [ o "out" unit ]
    "pixelate" -> Just $ "pixelate" /\ [ q "pixelX" $ Number 20.0, q "pixelY" $ Number 20.0 ] /\ [ o "out" unit ]
    "repeat" -> Just $ "repeat" /\ [ q "repeatX" $ Number 3.0, q "repeatY" $ Number 3.0, q "offsetX" $ Number 0.0, q "offsetY" $ Number 0.0 ] /\ [ o "out" unit ]
    "repeatX" -> Just $ "repeatX" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "repeatY" -> Just $ "repeatY" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "kaleid" -> Just $ "kaleid" /\ [ q "nSides" $ Number 4.0 ] /\ [ o "out" unit ]
    "scroll" -> Just $ "scroll" /\ [ q "scrollX" $ Number 0.5, q "scrollY" $ Number 0.5, q "speedX" $ Number 0.0, q "speedY" $ Number 0.0 ] /\ [ o "out" unit ]
    "scrollX" -> Just $ "scrollX" /\ [ q "scrollX" $ Number 0.5, q "speed" $ Number 0.0 ] /\ [ o "out" unit ]
    "scrollY" -> Just $ "scrollY" /\ [ q "scrollY" $ Number 0.5, q "speed" $ Number 0.0 ] /\ [ o "out" unit ]

    "posterize" -> Just $ "posterize" /\ [ q "bins" $ Number 3.0, q "gamma" $ Number 6.0 ] /\ [ o "out" unit ]
    "shift" -> Just $ "shift" /\ [ q "r" $ Number 0.5, q "g" $ Number 0.0, q "b" $ Number 0.0, q "a" $ Number 0.5 ] /\ [ o "out" unit ]
    "invert" -> Just $ "invert" /\ [ q "amount" $ Number 1.0 ] /\ [ o "out" unit ]
    "contrast" -> Just $ "contrast" /\ [ q "amount" $ Number 1.6 ] /\ [ o "out" unit ]
    "brightness" -> Just $ "brightness" /\ [ q "amount" $ Number 0.4 ] /\ [ o "out" unit ]
    "luma" -> Just $ "luma" /\ [ q "threshold" $ Number 0.5, q "tolerance" $ Number 0.1 ] /\ [ o "out" unit ]
    "thresh" -> Just $ "thresh" /\ [ q "threshold" $ Number 0.5, q "tolerance" $ Number 0.04 ] /\ [ o "out" unit ]
    "color" -> Just $ "color" /\ [ q "r" $ Number 1.0, q "g" $ Number 1.0, q "b" $ Number 1.0, q "a" $ Number 1.0 ] /\ [ o "out" unit ]
    "saturate" -> Just $ "saturate" /\ [ q "amount" $ Number 2.0 ] /\ [ o "out" unit ]
    "hue" -> Just $ "hue" /\ [ q "amount" $ Number 0.4 ] /\ [ o "out" unit ]
    "colorama" -> Just $ "colorama" /\ [ q "amount" $ Number 0.005 ] /\ [ o "out" unit ]
    -- "sum" -> Color
    "r" -> Just $ "r" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "g" -> Just $ "g" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "b" -> Just $ "b" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "a" -> Just $ "a" /\ [ q "scale" $ Number 1.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]

     -- FIXME : first arg is texture for everything below
    "add" -> Just $ "add" /\ [ q "amount" $ Number 1.0 ] /\ [ o "out" unit ]
    "sub" -> Just $ "sub" /\ [ q "amount" $ Number 1.0 ] /\ [ o "out" unit ]
    "layer" -> Just $ "layer" /\ [] /\ []
    "blend" -> Just $ "blend" /\ [ q "amount" $ Number 0.5 ] /\ [ o "out" unit ]
    "mult" -> Just $ "mult" /\ [ q "amount" $ Number 1.0 ] /\ [ o "out" unit ]
    "diff" -> Just $ "diff" /\ [] /\ []
    "mask" -> Just $ "mask" /\ [] /\ []

    "modulateRepeat" -> Just $ "modRepeat" /\ [ q "repeatX" $ Number 3.0, q "repeatY" $ Number 3.0, q "offsetX" $ Number 0.5, q "offsetY" $ Number 0.5 ] /\ [ o "out" unit ]
    "modulateRepeatX" -> Just $ "modRepeatX" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.5 ] /\ [ o "out" unit ]
    "modulateRepeatY" -> Just $ "modRepeatY" /\ [ q "reps" $ Number 3.0, q "offset" $ Number 0.5 ] /\ [ o "out" unit ]
    "modulateKaleid" -> Just $ "modKaleid" /\ [ q "nSides" $ Number 4.0 ] /\ [ o "out" unit ]
    "modulateScrollX" -> Just $ "modScrollX" /\ [ q "scrollX" $ Number 0.5, q "speed" $ Number 0.0 ] /\ [ o "out" unit ]
    "modulateScrollY" -> Just $ "modScrollY" /\ [ q "scrollY" $ Number 0.5, q "speed" $ Number 0.0 ] /\ [ o "out" unit ]
    "modulate" -> Just $ "modulate" /\ [ q "amount" $ Number 0.1 ] /\ [ o "out" unit ]
    "modulateScale" -> Just $ "modScale" /\ [ q "multiple" $ Number 1.0, q "offset" $ Number 1.0 ] /\ [ o "out" unit ]
    "modulatePixelate" -> Just $ "modPixelate" /\ [ q "multiple" $ Number 10.0, q "offset" $ Number 3.0 ] /\ [ o "out" unit ]
    "modulateRotate" -> Just $ "modRotate" /\ [ q "multiple" $ Number 1.0, q "offset" $ Number 0.0 ] /\ [ o "out" unit ]
    "modulateHue" -> Just $ "modHue" /\ [ q "amount" $ Number 1.0 ] /\ [ o "out" unit ]

    -- "initCam" -> ExternalSources
    -- "initImage" -> ExternalSources
    -- "initVideo" -> ExternalSources
    -- "init" -> ExternalSources
    -- "initStream" -> ExternalSources
    -- "initScreen" -> ExternalSources

    -- "render" -> Synth
    -- "update" -> Synth
    -- "setResolution" -> Synth
    -- "hush" -> Synth
    -- "setFunction" -> Synth
    -- "speed" -> Synth
    -- "bpm" -> Synth
    -- "width" -> Synth
    -- "height" -> Synth
    -- "time" -> Synth
    -- "mouse" -> Synth
    -- "pi" -> Synth

    -- "fft" -> Audio
    -- "setSmooth" -> Audio
    -- "setCutoff" -> Audio
    -- "setBins" -> Audio
    -- "setScale" -> Audio
    -- "hide" -> Audio
    -- "show" -> Audio

    -- "setScale" -> Audio

    -- "out" -> Out

    "linear" -> Just $ "linear" /\ [] /\ [ o "out" unit ]
    "fast" -> Just $ "fast" /\ [ q "v" $ Number 1.0 ] /\ [ o "out" unit ]
    "smooth" -> Just $ "smooth" /\ [ q "v" $ Number 1.0 ] /\ [ o "out" unit ]
    "fit" -> Just $ "fit" /\ [ q "low" $ Number 0.0, q "high" $ Number 1.0 ] /\ [ o "out" unit ]
    "offset" -> Just $ "offset" /\ [ q "v" $ Number 0.5 ] /\ [ o "out" unit ]
    -- "inOutCubic" -> Just $ "inOutCubic" /\ []

    _ -> Nothing


showUsingFn :: forall arg out x. Show arg => Show out => ToFn arg out x => Proxy arg -> Proxy out -> x -> String
showUsingFn _ _ a =
    case (toFn a :: String /\ Array (Fn.Argument arg) /\ Array (Fn.Output out)) of
        name /\ args /\ outs ->
            if Array.length args > 0 && Array.length outs > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (show <$> args) <> " -> " <> String.joinWith " " (show <$> outs) <> ">"
            else if Array.length args > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (show <$> args) <> ">"
            else if Array.length outs > 0 then
                "<" <> String.pascalCase name <> " -> " <> String.joinWith " " (show <$> outs) <> ">"
            else
                 "<" <> String.pascalCase name <> ">"


showUsingFnV :: forall x. ToFn Value Unit x => x -> String
showUsingFnV = showUsingFn (Proxy :: _ Value) (Proxy :: _ Unit)


showUsingFnTOrV :: forall x. ToFn TOrV OTOrV x => x -> String
showUsingFnTOrV = showUsingFn (Proxy :: _ TOrV) (Proxy :: _ OTOrV)


showUsingFnTOrV' :: forall x. ToFn TOrV Unit x => x -> String
showUsingFnTOrV' = showUsingFn (Proxy :: _ TOrV) (Proxy :: _ Unit)



-- showUsingFnArgOut :: forall x. ToFn FnArg FnOut x => x -> String
-- showUsingFnArgOut = showUsingFn (Proxy :: _ FnArg) (Proxy :: _ FnOut)


showUsingPossiblyFn :: forall arg out x. Show arg => Show out => PossiblyToFn arg out x => Proxy arg -> Proxy out -> (x -> String) -> x -> String
showUsingPossiblyFn _ _ fallback a =
    case (possiblyToFn a :: Maybe (String /\ Array (Fn.Argument arg) /\ Array (Fn.Output out))) of
        Just (name /\ args /\ outs ) ->
            if Array.length args > 0 && Array.length outs > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (show <$> args) <> " -> " <> String.joinWith " " (show <$> outs) <> ">"
            else if Array.length args > 0 then
                "<" <> String.pascalCase name <> " " <> String.joinWith " " (show <$> args) <> ">"
            else if Array.length outs > 0 then
                "<" <> String.pascalCase name <> " -> " <> String.joinWith " " (show <$> outs) <> ">"
            else
                 "<" <> String.pascalCase name <> ">"
        Nothing ->
            fallback a


showUsingPossiblyFnV :: forall x. PossiblyToFn Value Unit x  => (x -> String) -> x -> String
showUsingPossiblyFnV fallback a = showUsingPossiblyFn (Proxy :: _ Value) (Proxy :: _ Unit) fallback a


argSep :: String
argSep = ";"

argsEnd :: String
argsEnd = ";" -- FIXME: try different values here and run tests, seems parsing functions works only with this option and also doesn't consume all the output


texSep :: String
texSep = " % "

texsEnd :: String
texsEnd = " %"


unparsedFnStart :: String
unparsedFnStart = "¤■"

unparsedFnTerminals :: Array Char
unparsedFnTerminals = [ '¤', '■' ]

unparsedFnEnd :: String
unparsedFnEnd = "■¤"


jsexprStart :: String
jsexprStart = "```"

jsexprTerminals :: Array Char
jsexprTerminals = [ '`' ]

jsexprEnd :: String
jsexprEnd = "```"


glslMarker :: String
glslMarker = "<GLSL>"

glslStart :: String
glslStart = "¤¤¤¤■"

glslTerminals :: Array Char
glslTerminals = [ '¤', '■' ]

glslEnd :: String
glslEnd = "■¤¤¤¤"


encodeUsingFn :: forall a. ToFn Value Unit a => a -> String
encodeUsingFn a =
    case toFn a :: String /\ Array (Fn.Argument Value) /\ Array (Fn.Output Unit) of
        name /\ args /\ _ ->
            if Array.length args > 0 then
                String.toUpper name <> " " <> String.joinWith argSep (encode <$> Fn.argValue <$> args) <> argsEnd
            else
                String.toUpper name <> " " <> argsEnd
