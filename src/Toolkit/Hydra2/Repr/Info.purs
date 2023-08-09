module Toolkit.Hydra2.Repr.Info where

import Prelude

import Color (Color)
import Color as Color
import Data.Mark (class Mark)

import Data.Array as Array
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Repr as R -- (class ToRepr, class FromRepr, toRepr, fromRepr)


import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..)) as W


data InfoRepr = InfoRepr { short :: String, full :: String }


-- instance NMF.HasRepr a InfoRepr where
--     toRepr :: forall f i o. InNode f i o -> H.Value -> InfoRepr
--     toRepr _ a = InfoRepr

class ShortInfo a where
    short :: a -> String


class FullInfo a where
    full :: a -> String


instance ShortInfo InfoRepr where
    short (InfoRepr info) = info.short


instance FullInfo InfoRepr where
    full (InfoRepr info) = info.full


instance ShortInfo H.Value where
    short :: H.Value -> String
    short = case _ of
        H.None -> "-"
        H.Undefined -> "?"
        H.Number n -> show n
        H.Dep _ -> "→#"
        H.VArray vs ease -> short vs <> short ease
        H.Time -> "⏲" -- ⏰
        H.MouseX -> "MX"
        H.MouseY -> "MY"
        H.Width -> "↔" -- <->
        H.Height -> "↕" --
        H.Pi -> "π"
        H.Fft bin -> "🔈" <> short bin


instance ShortInfo Unit where
    short :: Unit -> String
    short = const "()"


instance ShortInfo H.Texture where
    short :: H.Texture -> String
    short = case _ of
        H.Empty -> "∅"
        H.Start from -> short from --"&" <> short from
        H.BlendOf { what, with } blend -> "BND" -- TODO: <> short blend
        H.Filter texture op -> "FLT" -- TODO: expand op
        H.ModulateWith texture mod -> "MOD" -- TODO: expand mod
        H.Geometry texture gmt -> "GMT" -- TODO: expand gmt


instance ShortInfo H.TODO where
    short :: H.TODO -> String
    short = const "☀"


instance ShortInfo H.Context where
    short :: H.Context -> String
    short = const "CTX"


instance ShortInfo H.UpdateFn where
    short :: H.UpdateFn -> String
    short = const "↦"


instance ShortInfo H.ExtSource where
    short :: H.ExtSource -> String
    short = case _ of
        H.Sketch _ -> "SKT"
        H.Video -> "VID"
        H.Camera _ -> "CAM" -- 🎥
        H.Shader _ -> "SHD"
        H.Unclear -> "???"


instance ShortInfo H.Source where
    short :: H.Source -> String
    short = case _ of
        H.Gradient _ -> "GRD"
        H.Noise _ -> "NZE"
        H.Osc _ -> "OSC"
        H.Shape _ -> "SHP"
        H.Solid _ -> "SLD"
        H.Voronoi _ -> "VRN"
        H.Load from -> short from
        H.External src _ -> short src


instance ShortInfo H.Url where
    short :: H.Url -> String
    short = const "🔗"


instance ShortInfo H.GlslFn where
    short :: H.GlslFn -> String
    short = const "↬"


instance ShortInfo H.SourceOptions where
    short :: H.SourceOptions -> String
    short = const "OPT"


instance ShortInfo H.Values where
    short :: H.Values -> String
    short (H.Values array) = "[" <> (show $ Array.length array) <> "]"


instance ShortInfo H.Ease where
    short :: H.Ease -> String
    short = case _ of
        H.Linear -> "╱"
        H.Fast _ -> "⏭"
        H.Smooth _ -> "↝"
        H.Fit _ -> "⇔"
        H.Offset _ -> "⏼"
        H.InOutCubic -> "⊰"


instance ShortInfo H.AudioSource where
    short :: H.AudioSource -> String
    short = case _ of
        H.Silence -> "⏹" -- 🔇
        H.Mic -> "" -- TODO
        H.File -> "" -- TODO


instance ShortInfo H.AudioBin where
    short :: H.AudioBin -> String
    short (H.AudioBin n) = "H" <> show n -- 🔈 🔉


instance ShortInfo H.OutputN where
    short :: H.OutputN -> String
    short = case _ of
        H.Output0 -> "⎑0" -- OUT0
        H.Output1 -> "⎑1" -- OUT1
        H.Output2 -> "⎑2" -- OUT2
        H.Output3 -> "⎑3" -- OUT3
        H.Output4 -> "⎑4" -- OUT4


instance ShortInfo H.SourceN where
    short :: H.SourceN -> String
    short = case _ of
        H.Source0 -> "S0"


instance ShortInfo H.RenderTarget where
    short :: H.RenderTarget -> String
    short = case _ of
        H.Four -> "∀"
        H.Output oN -> short oN


instance ShortInfo H.Fn where
    short :: H.Fn -> String
    short = case _ of
        H.VExpr vexpr -> "EXP"
        H.Fn code -> "PS"
        H.Unparsed str -> "STR"
        H.NoAction -> "--"


instance ShortInfo H.CanBeSource where
    short :: H.CanBeSource -> String
    short (H.CanBeSource cbs) = case cbs of
        Left sourceN -> short sourceN
        Right outputN -> short outputN


instance FullInfo H.Value where
    full :: H.Value -> String
    full = show


instance FullInfo Unit where
    full :: Unit -> String
    full = show


instance FullInfo H.Texture where
    full :: H.Texture -> String
    full = show


instance FullInfo H.TODO where
    full :: H.TODO -> String
    full = show


instance FullInfo H.Context where
    full :: H.Context -> String
    full = show


instance FullInfo H.UpdateFn where
    full :: H.UpdateFn -> String
    full = show


instance FullInfo H.Source where
    full :: H.Source -> String
    full = show


instance FullInfo H.Url where
    full :: H.Url -> String
    full = show


instance FullInfo H.GlslFn where
    full :: H.GlslFn -> String
    full = show


instance FullInfo H.SourceOptions where
    full :: H.SourceOptions -> String
    full = show


instance FullInfo H.Values where
    full :: H.Values -> String
    full = show


instance FullInfo H.Ease where
    full :: H.Ease -> String
    full = show


instance FullInfo H.AudioSource where
    full :: H.AudioSource -> String
    full = show


instance FullInfo H.AudioBin where
    full :: H.AudioBin -> String
    full = show


instance FullInfo H.OutputN where
    full :: H.OutputN -> String
    full = show


instance FullInfo H.SourceN where
    full :: H.SourceN -> String
    full = show


instance FullInfo H.ExtSource where
    full :: H.ExtSource -> String
    full = show


instance FullInfo H.Fn where
    full :: H.Fn -> String
    full = show


instance FullInfo H.RenderTarget where
    full :: H.RenderTarget -> String
    full = show


instance FullInfo H.CanBeSource where
    full :: H.CanBeSource -> String
    full = show



instance (ShortInfo a, FullInfo a) => NMF.HasRepr a InfoRepr where
    toRepr :: forall f i o. InNode f i o -> a -> InfoRepr
    toRepr _ a = InfoRepr { short : short a, full : full a }


instance (ShortInfo a, FullInfo a) => R.ToRepr a InfoRepr where
    toRepr :: a -> Maybe (R.Repr InfoRepr)
    toRepr = R.exists <<< InfoRepr <<< \a -> { short : short a, full : full a }


instance FullInfo W.WrapRepr where
    full :: W.WrapRepr -> String
    full = case _ of
        W.Value v -> full v
        W.Unit u -> full u
        W.Texture tex -> full tex
        W.OutputN on -> full on
        W.SourceN sn -> full sn
        W.TODO todo -> full todo
        W.Context ctx -> full ctx
        W.UpdateFn fn -> full fn
        W.Source src -> full src
        W.Url url -> full url
        W.GlslFn glsl -> full glsl
        W.SourceOptions opts -> full opts
        W.Values vals -> full vals
        W.Ease ease -> full ease
        W.Audio audio -> full audio
        W.AudioBin bin -> full bin
        W.ExtSource ext -> full ext
        W.Fn fn -> full fn
        W.Target trg -> full trg
        W.CBS cbs -> full cbs


instance ShortInfo W.WrapRepr where
    short :: W.WrapRepr -> String
    short = case _ of
        W.Value v -> short v
        W.Unit u -> short u
        W.Texture tex -> short tex
        W.OutputN on -> short on
        W.SourceN sn -> short sn
        W.TODO todo -> short todo
        W.Context ctx -> short ctx
        W.UpdateFn fn -> short fn
        W.Source src -> short src
        W.Url url -> short url
        W.GlslFn glsl -> short glsl
        W.SourceOptions opts -> short opts
        W.Values vals -> short vals
        W.Ease ease -> short ease
        W.Audio audio -> short audio
        W.AudioBin bin -> short bin
        W.ExtSource ext -> short ext
        W.Fn fn -> short fn
        W.Target trg -> short trg
        W.CBS cbs -> short cbs