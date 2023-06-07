module Toolkit.Hydra2.Repr.Info where

import Prelude

import Color (Color)
import Color as Color
import Data.Mark (class Mark)

import Data.Array as Array
import Data.String as String
import Data.Maybe (Maybe(..))
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
        H.Required -> "REQ"
        H.Number n -> show n
        H.Dep _ -> "→#"
        H.VArray vs ease -> short vs <> short ease
        H.Time -> "⏲" -- ⏰
        H.MouseX -> "MX"
        H.MouseY -> "MY"
        H.Width -> "↔" -- <->
        H.Height -> "↕" --
        H.Pi -> "π"
        H.Audio audio bin -> short audio <> short bin


instance ShortInfo Unit where
    short :: Unit -> String
    short = const "()"


instance ShortInfo H.Texture where
    short :: H.Texture -> String
    short = case _ of
        H.Empty -> "∅"
        H.From from -> "&" <> short from
        H.BlendOf { what, with } blend -> "BND" -- TODO: <> short blend
        H.WithColor texture op -> "CLR" -- TODO: expand op
        H.ModulateWith texture mod -> "MOD" -- TODO: expand mod
        H.Geometry texture gmt -> "GMT" -- TODO: expand gmt


instance ShortInfo H.From where
    short :: H.From -> String
    short = case _ of
        H.All -> "∀"
        H.Output out -> short out


instance ShortInfo H.TODO where
    short :: H.TODO -> String
    short = const "☀"


instance ShortInfo H.Context where
    short :: H.Context -> String
    short = const "CTX"


instance ShortInfo H.UpdateFn where
    short :: H.UpdateFn -> String
    short = const "↦"


instance ShortInfo H.Source where
    short :: H.Source -> String
    short = case _ of
        H.Dynamic -> "DYN"
        H.Video -> "VID"
        H.Gradient _ -> "GRD"
        H.Camera -> "CAM" -- 🎥
        H.Noise _ -> "NZE"
        H.Osc _ -> "OSC"
        H.Shape _ -> "SHP"
        H.Solid _ -> "SLD"
        H.Source from -> short from
        H.Voronoi _ -> "VRN"


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


instance ShortInfo H.Audio where
    short :: H.Audio -> String
    short = case _ of
        H.Silence -> "⏹" -- 🔇
        H.Mic -> ""
        H.File -> ""


instance ShortInfo H.AudioBin where
    short :: H.AudioBin -> String
    short = case _ of
        H.H0 -> "H0" -- 🔈 🔉
        H.H1 -> "H1"
        H.H2 -> "H2"
        H.H3 -> "H3"
        H.H4 -> "H4"


instance ShortInfo H.Output where
    short :: H.Output -> String
    short = case _ of
        H.Screen -> "SCR"
        H.Output0 -> "OUT0" -- ⎑0
        H.Output1 -> "OUT1" -- ⎑1
        H.Output2 -> "OUT2" -- ⎑2



instance FullInfo H.Value where
    full :: H.Value -> String
    full = case _ of
        H.None -> "None"
        H.Required -> "Required"
        H.Number n -> show n
        H.VArray vals ease -> full vals <> " at " <> full ease
        H.Dep _ -> "Dep"
        H.Time -> "Time"
        H.MouseX -> "Mouse X"
        H.MouseY -> "Mouse Y"
        H.Width -> "Width"
        H.Height -> "Height"
        H.Pi -> "Pi"
        H.Audio audio bin -> full audio <> " @ " <> full bin


instance FullInfo Unit where
    full :: Unit -> String
    full _ = "Unit"


instance FullInfo H.Texture where
    full :: H.Texture -> String
    full = case _ of
        H.Empty -> "Empty"
        H.From src -> "From " <> full src
        H.BlendOf { what, with } blend -> "BND" -- TODO: <> short blend
        H.WithColor texture op -> "CLR" -- TODO: expand op
        H.ModulateWith texture mod -> "MOD" -- TODO: expand mod
        H.Geometry texture gmt -> "GMT" -- TODO: expand gmt


instance FullInfo H.From where
    full :: H.From -> String
    full = short -- TODO


instance FullInfo H.TODO where
    full :: H.TODO -> String
    full = const "TODO"


instance FullInfo H.Context where
    full :: H.Context -> String
    full = const "Context" -- TODO


instance FullInfo H.UpdateFn where
    full :: H.UpdateFn -> String
    full = short -- TODO


instance FullInfo H.Source where
    full :: H.Source -> String
    full = short -- TODO


instance FullInfo H.Url where
    full :: H.Url -> String
    full = short -- TODO


instance FullInfo H.GlslFn where
    full :: H.GlslFn -> String
    full = short -- TODO


instance FullInfo H.SourceOptions where
    full :: H.SourceOptions -> String
    full = short -- TODO


instance FullInfo H.Values where
    full :: H.Values -> String
    full (H.Values array) = "[" <> String.joinWith "," (full <$> array) <> "]"


instance FullInfo H.Ease where
    full :: H.Ease -> String
    full = short -- TODO


instance FullInfo H.Audio where
    full :: H.Audio -> String
    full = short -- TODO


instance FullInfo H.AudioBin where
    full :: H.AudioBin -> String
    full = short -- TODO


instance FullInfo H.Output where
    full :: H.Output -> String
    full = short -- TODO



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
        W.From f -> full f
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
        W.Output out -> full out


instance ShortInfo W.WrapRepr where
    short :: W.WrapRepr -> String
    short = case _ of
        W.Value v -> short v
        W.Unit u -> short u
        W.Texture tex -> short tex
        W.From f -> short f
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
        W.Output out -> short out