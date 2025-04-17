module HydraTk.Repr.Show where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String (joinWith) as String
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Array (length) as Array

import Noodle.Fn.Signature (Signature, toSignature)
import Noodle.Fn.Signature as Sig

import HydraTk.Types as H
import HydraTk.Repr.Target


class HydraShow a where -- Some kind of `ToCode` ?
    hShow :: a -> String


instance (HydraShow arg, HydraShow out) => HydraShow (Signature arg out) where
    hShow = Sig._showManually hShow hShow


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

instance HydraShow H.TOrV where
    hShow = case _ of
        H.T tex -> hShow tex
        H.V val -> hShow val


instance HydraShow H.OTOrV where
    hShow = case _ of
        H.OT -> "TEX"
        H.OV -> "VAL"


instance HydraShow H.JsExpr where
    hShow :: H.JsExpr -> String
    hShow = case _ of
        H.Val value -> hShow value
        H.AddE v1 v2 -> hShow v1 <> " + " <> hShow v2
        H.SubE v1 v2 -> hShow v1 <> " - " <> hShow v2
        H.MulE v1 v2 -> hShow v1 <> " * " <> hShow v2
        H.DivE v1 v2 -> hShow v1 <> " / " <> hShow v2
        H.ModE v1 v2 -> hShow v1 <> " % " <> hShow v2
        H.Math meth maybeExpr ->
            "Math." <> show meth <>
                (case maybeExpr of
                    Just expr -> "(" <> hShow expr <> ")"
                    Nothing -> ""
                )
        H.Brackets expr -> "( " <> hShow expr <> " )"


instance HydraShow H.DepFn where
    hShow :: H.DepFn -> String
    hShow = case _ of
        H.UserExpr jsexpr -> ":: " <> hShow jsexpr <> " ::"
        H.DepFn _ -> "[Code]"
        H.Unparsed str -> "{{ " <> str <> " }}"
        H.NoAction -> "--"


instance HydraShow H.Value where
    hShow :: H.Value -> String
    hShow = case _ of
        H.None -> "<None>"
        H.Undefined -> "<Undefined>"
        H.Number n -> "#" <> show n
        H.VArray vals ease -> "<" <> hShow vals <> " at " <> hShow ease <> ">"
        H.Dep fn -> "<Dep " <> hShow fn <> ">"
        H.Time -> "<Time>"
        H.MouseX -> "<Mouse X>"
        H.MouseY -> "<Mouse Y>"
        H.Width -> "<Width>"
        H.Height -> "<Height>"
        H.Pi -> "<Pi>"
        H.Fft bin -> "<@ " <> hShow bin <> ">"


instance HydraShow H.Texture where
    hShow :: H.Texture -> String
    hShow = case _ of
        H.Empty -> "∅" -- "?"
        H.Start src -> "• " <> hShow src
        H.BlendOf { what, with } blend -> hShow with <> " + " <> hShow what <> " >~ ░ " <> hShow blend
        H.Filter texture op -> hShow texture <> " >~ ƒ " <> hShow op
        H.ModulateWith { what, with } mod -> hShow with <> " + " <> hShow what <> " >~ ¤ " <> hShow mod
        H.Geometry texture gmt -> hShow texture <> " >~ ■ " <> hShow gmt
        H.CallGlslFn { mbWith, over } glslFn ->
            case mbWith of
                Just with -> hShow with <> " | " <> hShow over <> " >~ $ " <> hShow glslFn
                Nothing -> hShow over <> " >~ $ " <> hShow glslFn


instance HydraShow H.Blend where
    hShow :: H.Blend -> String
    hShow blend = hShow $ (toSignature hydraV blend :: Signature H.Value Unit)


instance HydraShow H.ColorOp where
    hShow :: H.ColorOp -> String
    hShow colorOp = hShow $ (toSignature hydraV colorOp :: Signature H.Value Unit)


instance HydraShow H.Modulate where
    hShow :: H.Modulate -> String
    hShow mod = hShow $ (toSignature hydraV mod :: Signature H.Value Unit)


instance HydraShow H.Geometry where
    hShow :: H.Geometry -> String
    hShow geo = hShow $ (toSignature hydraV geo :: Signature H.Value Unit)


instance HydraShow H.TODO where
    hShow :: H.TODO -> String
    hShow = const "TODO"


instance HydraShow H.Context where
    hShow :: H.Context -> String
    hShow (H.Context { time }) = "Context { " <> show time <> " }"


instance HydraShow H.UpdateFn where
    hShow ::H.UpdateFn -> String
    hShow = const "Update Function" -- TODO


instance HydraShow H.From where
    hShow :: H.From -> String
    hShow = -- showUsingFnV
        case _ of
            H.Gradient { speed } -> "Gradient " <> hShow speed
            H.Noise { scale, offset } -> "Noise " <> hShow scale <> " " <> hShow offset
            H.Osc { frequency, sync, offset } -> "Osc " <> hShow frequency <> " " <> hShow sync <> " " <> hShow offset
            H.Shape { sides, radius, smoothing } -> "Shape " <> hShow sides <> " " <> hShow radius <> " " <> hShow smoothing
            H.Solid { r, g, b, a } -> "Solid " <> hShow r <> " " <> hShow g <> " " <> hShow b <> " " <> hShow a
            H.Voronoi { scale, speed, blending } -> "Voronoi " <> hShow scale <> " " <> hShow speed <> " " <> hShow blending


instance HydraShow H.Source where
    hShow :: H.Source -> String
    hShow = case _ of
        H.From from -> hShow from
        H.Load outputN -> "Load " <> hShow outputN
        H.External sourceN ext -> "External " <> hShow sourceN <> " " <> hShow ext


instance HydraShow H.Url where
    hShow :: H.Url -> String
    hShow (H.Url url) = "Url: " <> show url


instance HydraShow H.GlslFnKind where
    hShow :: H.GlslFnKind -> String
    hShow = case _ of
        H.FnSrc -> "Source"
        H.FnCoord -> "Coord"
        H.FnCombineCoord -> "CombineCoord"
        H.FnCombine -> "Combine"
        H.FnColor -> "Color"


instance HydraShow H.GlslFn where
    hShow :: H.GlslFn -> String
    hShow (H.GlslFn { kind, fn }) = "Define {" <> hShow kind <> "} " <> hShow fn


instance HydraShow H.GlslFnRef where
    hShow :: H.GlslFnRef -> String
    hShow (H.GlslFnRef fn) = "Call " <> hShow fn


instance HydraShow H.SourceOptions where
    hShow :: H.SourceOptions -> String
    hShow (H.SourceOptions { src }) = "Source Options { " {- TODO : <> show src -} <> " }"


instance HydraShow H.Values where
    hShow :: H.Values -> String
    hShow (H.Values array) = "[" <> String.joinWith "," (hShow <$> array) <> "]"


instance HydraShow H.Ease where
    hShow :: H.Ease -> String
    hShow = case _ of
        H.Ease H.Linear -> "Linear"
        H.Ease H.InOutCubic -> "InOutCubic"
        H.Fast v -> "Fast " <> hShow v
        H.Smooth v -> "Smooth " <> hShow v
        H.Fit { low, high } -> "Fit " <> hShow low <> " < " <> hShow high
        H.Offset v -> "Offset " <> hShow v


instance HydraShow H.AudioSource where
    hShow :: H.AudioSource -> String
    hShow = case _ of
        H.Silence -> "Silence"
        H.Mic -> "Microphone"
        H.File -> "File"


instance HydraShow H.AudioBin where
    hShow :: H.AudioBin -> String
    hShow (H.AudioBin n) = "@" <> show n


instance HydraShow H.SourceN where
    hShow :: H.SourceN -> String
    hShow = case _ of
        H.Source0 -> "Source 0"


instance HydraShow H.ExtSource where
    hShow :: H.ExtSource -> String
    hShow = case _ of
        H.Camera n -> "Camera " <> show n -- 🎥
        H.Sketch name -> "Sketch " <> name
        H.Video -> "Video"
        H.Unclear -> "Unclear"


instance HydraShow H.CanBeSource where
    hShow :: H.CanBeSource -> String
    hShow (H.CanBeSource cbs) = case cbs of
        Left sourceN  -> hShow sourceN
        Right outputN -> hShow outputN


instance HydraShow H.OutputN where
    hShow :: H.OutputN -> String
    hShow = case _ of
        H.Output0 -> "Output 0"
        H.Output1 -> "Output 1"
        H.Output2 -> "Output 2"
        H.Output3 -> "Output 3"
        H.Output4 -> "Output 4"


instance HydraShow H.RenderTarget where
    hShow :: H.RenderTarget -> String
    hShow H.Four = "Four"
    hShow (H.Output oN) = hShow oN



type HChannelLabel = String


class HydraToChannelLabel a where
    toChannelLabel :: a -> HChannelLabel


instance HydraToChannelLabel H.Value where
    toChannelLabel :: H.Value -> String
    toChannelLabel = case _ of
        H.None -> "-"
        H.Undefined -> "?"
        H.Number n -> show n
        H.Dep (H.UserExpr _) -> "→{}"
        H.Dep (H.DepFn _) -> "→⨐"
        H.Dep (H.Unparsed _) -> "→⍟"
        H.Dep H.NoAction -> "→∅"
        H.VArray vs ease -> toChannelLabel vs <> toChannelLabel ease
        H.Time -> "⏲" -- ⏰
        H.MouseX -> "MX"
        H.MouseY -> "MY"
        H.Width -> "↔" -- <->
        H.Height -> "↕" --
        H.Pi -> "π"
        H.Fft bin -> "🔈" <> toChannelLabel bin


instance HydraToChannelLabel Unit where
    toChannelLabel :: Unit -> HChannelLabel
    toChannelLabel = const "()"


instance HydraToChannelLabel H.Texture where
    toChannelLabel :: H.Texture -> HChannelLabel
    toChannelLabel = case _ of
        H.Empty -> "∅"
        H.Start from -> toChannelLabel from --"&" <> short from
        H.BlendOf { what, with } blend -> "BND" -- TODO: <> short blend
        H.Filter texture op -> "FLT" -- TODO: expand op
        H.ModulateWith texture mod -> "MOD" -- TODO: expand mod
        H.Geometry texture gmt -> "GMT" -- TODO: expand gmt
        H.CallGlslFn texture glsl -> "GLS" -- TODO: expand fn


instance HydraToChannelLabel H.TODO where
    toChannelLabel :: H.TODO -> HChannelLabel
    toChannelLabel = const "☀"


instance HydraToChannelLabel H.Context where
    toChannelLabel :: H.Context -> HChannelLabel
    toChannelLabel = const "CTX"


instance HydraToChannelLabel H.UpdateFn where
    toChannelLabel :: H.UpdateFn -> HChannelLabel
    toChannelLabel = const "↦"


instance HydraToChannelLabel H.ExtSource where
    toChannelLabel :: H.ExtSource -> HChannelLabel
    toChannelLabel = case _ of
        H.Sketch _ -> "SKT"
        H.Video -> "VID"
        H.Camera _ -> "CAM" -- 🎥
        H.Unclear -> "???"


instance HydraToChannelLabel H.From where
    toChannelLabel :: H.From -> HChannelLabel
    toChannelLabel = case _ of
        H.Gradient _ -> "GRD"
        H.Noise _ -> "NZE"
        H.Osc _ -> "OSC"
        H.Shape _ -> "SHP"
        H.Solid _ -> "SLD"
        H.Voronoi _ -> "VRN"


instance HydraToChannelLabel H.Source where
    toChannelLabel :: H.Source -> HChannelLabel
    toChannelLabel = case _ of
        H.From from -> toChannelLabel from
        H.Load from -> toChannelLabel from
        H.External src _ -> toChannelLabel src


instance HydraToChannelLabel H.Url where
    toChannelLabel :: H.Url -> HChannelLabel
    toChannelLabel = const "🔗" -- '#'


instance HydraToChannelLabel H.GlslFn where
    toChannelLabel :: H.GlslFn -> HChannelLabel
    toChannelLabel = const "↬"


instance HydraToChannelLabel H.SourceOptions where
    toChannelLabel :: H.SourceOptions -> HChannelLabel
    toChannelLabel = const "OPT"


instance HydraToChannelLabel H.Values where
    toChannelLabel :: H.Values -> HChannelLabel
    toChannelLabel (H.Values array) = "[" <> (show $ Array.length array) <> "]"


instance HydraToChannelLabel H.Ease where
    toChannelLabel :: H.Ease -> HChannelLabel
    toChannelLabel = case _ of
        H.Ease H.Linear -> "╱"
        H.Ease H.InOutCubic -> "⊰"
        H.Fast _ -> "⏭"
        H.Smooth _ -> "↝"
        H.Fit _ -> "⇔"
        H.Offset _ -> "⏼"


instance HydraToChannelLabel H.AudioSource where
    toChannelLabel :: H.AudioSource -> HChannelLabel
    toChannelLabel = case _ of
        H.Silence -> "⏹" -- 🔇
        H.Mic -> "" -- TODO
        H.File -> "" -- TODO


instance HydraToChannelLabel H.AudioBin where
    toChannelLabel :: H.AudioBin -> HChannelLabel
    toChannelLabel (H.AudioBin n) = "H" <> show n -- 🔈 🔉


instance HydraToChannelLabel H.OutputN where
    toChannelLabel :: H.OutputN -> HChannelLabel
    toChannelLabel = case _ of
        H.Output0 -> "⎑0" -- OUT0
        H.Output1 -> "⎑1" -- OUT1
        H.Output2 -> "⎑2" -- OUT2
        H.Output3 -> "⎑3" -- OUT3
        H.Output4 -> "⎑4" -- OUT4


instance HydraToChannelLabel H.SourceN where
    toChannelLabel :: H.SourceN -> HChannelLabel
    toChannelLabel = case _ of
        H.Source0 -> "S0"


instance HydraToChannelLabel H.RenderTarget where
    toChannelLabel :: H.RenderTarget -> HChannelLabel
    toChannelLabel = case _ of
        H.Four -> "∀"
        H.Output oN -> toChannelLabel oN


instance HydraToChannelLabel H.DepFn where
    toChannelLabel :: H.DepFn -> HChannelLabel
    toChannelLabel = case _ of
        H.UserExpr jsexpr -> "EXP"
        H.DepFn code -> "PS"
        H.Unparsed str -> "STR"
        H.NoAction -> "--"


instance HydraToChannelLabel H.CanBeSource where
    toChannelLabel :: H.CanBeSource -> HChannelLabel
    toChannelLabel (H.CanBeSource cbs) = case cbs of
        Left sourceN -> toChannelLabel sourceN
        Right outputN -> toChannelLabel outputN


instance HydraToChannelLabel H.TOrV where
    toChannelLabel :: H.TOrV -> HChannelLabel
    toChannelLabel (H.T tex) = toChannelLabel tex
    toChannelLabel (H.V val) = toChannelLabel val