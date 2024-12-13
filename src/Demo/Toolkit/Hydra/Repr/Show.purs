module Hydra.Repr.Show where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String (joinWith) as String
import Data.Newtype (class Newtype, unwrap, wrap)

import Noodle.Fn.ToFn (Fn, toFn)
import Noodle.Fn.ToFn as Fn

import Hydra.Types
import Hydra.Repr.Target


class HydraShow a where -- Some kind of `ToCode` ?
    hShow :: a -> String


instance (HydraShow arg, HydraShow out) => HydraShow (Fn arg out) where
    hShow = Fn._showManually hShow hShow


instance HydraShow Unit where
    hShow = const "<>"


{-
else instance (Show arg, HydraShow out) => HydraShow (Fn arg out) where
    hShow = Fn._showManually show hShow
else instance (HydraShow arg, Show out) => HydraShow (Fn arg out) where
    hShow = Fn._showManually hShow show
else instance (Show arg, Show out) => HydraShow (Fn arg out) where
    hShow = show
-}

instance HydraShow TOrV where
    hShow = case _ of
        T tex -> hShow tex
        V val -> hShow val


instance HydraShow OTOrV where
    hShow = case _ of
        OT -> "TEX"
        OV -> "VAL"


instance HydraShow JsExpr where
    hShow :: JsExpr -> String
    hShow = case _ of
        Val value -> hShow value
        AddE v1 v2 -> hShow v1 <> " + " <> hShow v2
        SubE v1 v2 -> hShow v1 <> " - " <> hShow v2
        MulE v1 v2 -> hShow v1 <> " * " <> hShow v2
        DivE v1 v2 -> hShow v1 <> " / " <> hShow v2
        ModE v1 v2 -> hShow v1 <> " % " <> hShow v2
        Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> hShow expr <> ")"
                    Nothing -> ""
                )
        Brackets expr -> "( " <> hShow expr <> " )"


instance HydraShow DepFn where
    hShow :: DepFn -> String
    hShow = case _ of
        UserExpr jsexpr -> ":: " <> hShow jsexpr <> " ::"
        DepFn _ -> "[Code]"
        Unparsed str -> "{{ " <> str <> " }}"
        NoAction -> "--"


instance HydraShow Value where
    hShow :: Value -> String
    hShow = case _ of
        None -> "<None>"
        Undefined -> "<Undefined>"
        Number n -> "#" <> show n
        VArray vals ease -> "<" <> hShow vals <> " at " <> hShow ease <> ">"
        Dep fn -> "<Dep " <> hShow fn <> ">"
        Time -> "<Time>"
        MouseX -> "<Mouse X>"
        MouseY -> "<Mouse Y>"
        Width -> "<Width>"
        Height -> "<Height>"
        Pi -> "<Pi>"
        Fft bin -> "<@ " <> hShow bin <> ">"


instance HydraShow Texture where
    hShow :: Texture -> String
    hShow = case _ of
        Empty -> "∅" -- "?"
        Start src -> "• " <> hShow src
        BlendOf { what, with } blend -> hShow with <> " + " <> hShow what <> " >~  ░ " <> hShow blend
        Filter texture op -> hShow texture <> " >~ ƒ " <> hShow op
        ModulateWith { what, with } mod -> hShow with <> " + " <> hShow what <> " >~ ¤ " <> hShow mod
        Geometry texture gmt -> hShow texture <> " >~ ■ " <> hShow gmt
        CallGlslFn { mbWith, over } glslFn ->
            case mbWith of
                Just with -> hShow with <> " | " <> hShow over <> " >~ $ " <> hShow glslFn
                Nothing -> hShow over <> " >~ $ " <> hShow glslFn


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


instance HydraShow Blend where
    hShow :: Blend -> String
    hShow blend = hShow $ (toFn hydraV blend :: Fn Value Unit)


instance HydraShow ColorOp where
    hShow :: ColorOp -> String
    hShow colorOp = hShow $ (toFn hydraV colorOp :: Fn Value Unit)


instance HydraShow Modulate where
    hShow :: Modulate -> String
    hShow mod = hShow $ (toFn hydraV mod :: Fn Value Unit)


instance HydraShow Geometry where
    hShow :: Geometry -> String
    hShow geo = hShow $ (toFn hydraV geo :: Fn Value Unit)


instance HydraShow TODO where
    hShow :: TODO -> String
    hShow = const "TODO"


instance HydraShow Context where
    hShow :: Context -> String
    hShow (Context { time }) = "Context { " <> show time <> " }"


instance HydraShow UpdateFn where
    hShow :: UpdateFn -> String
    hShow = const "Update Function" -- TODO


instance HydraShow From where
    hShow :: From -> String
    hShow = -- showUsingFnV
        case _ of
            Gradient { speed } -> "Gradient " <> hShow speed
            Noise { scale, offset } -> "Noise " <> hShow scale <> " " <> hShow offset
            Osc { frequency, sync, offset } -> "Osc " <> hShow frequency <> " " <> hShow sync <> " " <> hShow offset
            Shape { sides, radius, smoothing } -> "Shape " <> hShow sides <> " " <> hShow radius <> " " <> hShow smoothing
            Solid { r, g, b, a } -> "Solid " <> hShow r <> " " <> hShow g <> " " <> hShow b <> " " <> hShow a
            Voronoi { scale, speed, blending } -> "Voronoi " <> hShow scale <> " " <> hShow speed <> " " <> hShow blending


instance HydraShow Source where
    hShow :: Source -> String
    hShow = case _ of
        From from -> hShow from
        Load outputN -> "Load " <> hShow outputN
        External sourceN ext -> "External " <> hShow sourceN <> " " <> hShow ext


instance HydraShow Url where
    hShow :: Url -> String
    hShow (Url url) = "Url: " <> show url


instance HydraShow GlslFnKind where
    hShow :: GlslFnKind -> String
    hShow = case _ of
        FnSrc -> "Source"
        FnCoord -> "Coord"
        FnCombineCoord -> "CombineCoord"
        FnCombine -> "Combine"
        FnColor -> "Color"


instance HydraShow GlslFn where
    hShow :: GlslFn -> String
    hShow (GlslFn { kind, fn }) = "Define {" <> hShow kind <> "} " <> hShow fn


instance HydraShow GlslFnRef where
    hShow :: GlslFnRef -> String
    hShow (GlslFnRef fn) = "Call " <> hShow fn


instance HydraShow SourceOptions where
    hShow :: SourceOptions -> String
    hShow (SourceOptions { src }) = "Source Options { " {- TODO : <> show src -} <> " }"


instance HydraShow Values where
    hShow :: Values -> String
    hShow (Values array) = "[" <> String.joinWith "," (hShow <$> array) <> "]"


instance HydraShow Ease where
    hShow :: Ease -> String
    hShow = case _ of
        Linear -> "Linear"
        Fast v -> "Fast " <> hShow v
        Smooth v -> "Smooth " <> hShow v
        Fit { low, high } -> "Fit " <> hShow low <> " < " <> hShow high
        Offset v -> "Offset " <> hShow v
        InOutCubic -> "InOutCubic"


instance HydraShow AudioSource where
    hShow :: AudioSource -> String
    hShow = case _ of
        Silence -> "Silence"
        Mic -> "Microphone"
        File -> "File"


instance HydraShow AudioBin where
    hShow :: AudioBin -> String
    hShow (AudioBin n) = "@" <> show n


instance HydraShow SourceN where
    hShow :: SourceN -> String
    hShow = case _ of
        Source0 -> "Source 0"


instance HydraShow ExtSource where
    hShow :: ExtSource -> String
    hShow = case _ of
        Camera n -> "Camera " <> show n -- 🎥
        Sketch name -> "Sketch " <> name
        Video -> "Video"
        Unclear -> "Unclear"


instance HydraShow CanBeSource where
    hShow :: CanBeSource -> String
    hShow (CanBeSource cbs) = case cbs of
        Left sourceN  -> hShow sourceN
        Right outputN -> hShow outputN


instance HydraShow OutputN where
    hShow :: OutputN -> String
    hShow = case _ of
        Output0 -> "Output 0"
        Output1 -> "Output 1"
        Output2 -> "Output 2"
        Output3 -> "Output 3"
        Output4 -> "Output 4"


instance HydraShow RenderTarget where
    hShow :: RenderTarget -> String
    hShow Four = "Four"
    hShow (Output oN) = hShow oN
