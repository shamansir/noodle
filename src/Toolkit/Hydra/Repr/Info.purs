module Tookit.Hydra.Repr.Info where

import Prelude

import Color (Color)
import Color as Color
import Data.Mark (class Mark)
import Data.SProxy (reflect')

import Data.Array as Array
-- import Data.String as String
-- import Data.String.CodePoints as SCP
-- import Data.String.CodeUnits as SCU
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr as R -- (class ToRepr, class FromRepr, toRepr, fromRepr)
-- import Data.FastVect.FastVect (Vect, (:), empty)
-- import Data.FastVect.FastVect as FV
-- import Data.FastVect.Common (term)
-- import Typelevel.Arithmetic.Add (Term, term)

import CompArts.Product as CAI

import Noodle.Id (Family(..), FamilyR(..))
import Noodle.Node.MapsFolds.Repr as NMF
import Noodle.Node.Path (InNode)

import Tookit.Hydra.Group as HG
import Tookit.Hydra.Types as H
import Tookit.Hydra.Repr.Wrap (WrapRepr(..)) as W


data InfoRepr = InfoRepr { shortLabel :: VShortChannelLabel, statusLine :: VStatusLine }


type VShortChannelLabel = String -- Vect 4 (Maybe SCP.CodePoint) -- ShortChannelValueLabel


type VStatusLine = String -- TODO: Tagged


{-
vshortLabel :: forall a. Show a => Char -> a -> Char /\ Char /\ Char /\ Char
vshortLabel filler a =
    case SCU.toCharArray $ SCU.takeRight 4 $ show a of
        [] -> filler /\ filler /\ filler /\ filler
        [ c1 ] -> filler /\ filler /\ filler /\ c1
        [ c1, c2 ] -> filler /\ filler /\ c1 /\ c2
        [ c1, c2, c3 ] -> filler /\ c1 /\ c2 /\ c3
        [ c1, c2, c3, c4 ] -> c1 /\ c2 /\ c3 /\ c4
        _ -> filler /\ filler /\ filler /\ filler
-}


class ShortChannelLabel a where
    shortLabel :: a -> VShortChannelLabel


class StatusLineInfo a where
    statusLine :: a -> VStatusLine -- Tagged


class Documentation a where
    docs :: a -> String


instance ShortChannelLabel InfoRepr where
    shortLabel (InfoRepr info) = info.shortLabel


instance StatusLineInfo InfoRepr where
    statusLine (InfoRepr info) = info.statusLine


instance ShortChannelLabel H.Value where
    shortLabel :: H.Value -> VShortChannelLabel
    shortLabel = case _ of
        H.None -> "-"
        H.Undefined -> "?"
        H.Number n -> show n
        H.Dep (H.UserExpr _) -> "→{}"
        H.Dep (H.Fn _) -> "→⨐"
        H.Dep (H.Unparsed _) -> "→⍟"
        H.Dep H.NoAction -> "→∅"
        H.VArray vs ease -> shortLabel vs <> shortLabel ease
        H.Time -> "⏲" -- ⏰
        H.MouseX -> "MX"
        H.MouseY -> "MY"
        H.Width -> "↔" -- <->
        H.Height -> "↕" --
        H.Pi -> "π"
        H.Fft bin -> "🔈" <> shortLabel bin


instance ShortChannelLabel Unit where
    shortLabel :: Unit -> VShortChannelLabel
    shortLabel = const "()"


instance ShortChannelLabel H.Texture where
    shortLabel :: H.Texture -> VShortChannelLabel
    shortLabel = case _ of
        H.Empty -> "∅"
        H.Start from -> shortLabel from --"&" <> short from
        H.BlendOf { what, with } blend -> "BND" -- TODO: <> short blend
        H.Filter texture op -> "FLT" -- TODO: expand op
        H.ModulateWith texture mod -> "MOD" -- TODO: expand mod
        H.Geometry texture gmt -> "GMT" -- TODO: expand gmt
        H.CallGlslFn texture glsl -> "GLS" -- TODO: expand fn


instance ShortChannelLabel H.TODO where
    shortLabel :: H.TODO -> VShortChannelLabel
    shortLabel = const "☀"


instance ShortChannelLabel H.Context where
    shortLabel :: H.Context -> VShortChannelLabel
    shortLabel = const "CTX"


instance ShortChannelLabel H.UpdateFn where
    shortLabel :: H.UpdateFn -> VShortChannelLabel
    shortLabel = const "↦"


instance ShortChannelLabel H.ExtSource where
    shortLabel :: H.ExtSource -> VShortChannelLabel
    shortLabel = case _ of
        H.Sketch _ -> "SKT"
        H.Video -> "VID"
        H.Camera _ -> "CAM" -- 🎥
        H.Unclear -> "???"


instance ShortChannelLabel H.Source where
    shortLabel :: H.Source -> VShortChannelLabel
    shortLabel = case _ of
        H.Gradient _ -> "GRD"
        H.Noise _ -> "NZE"
        H.Osc _ -> "OSC"
        H.Shape _ -> "SHP"
        H.Solid _ -> "SLD"
        H.Voronoi _ -> "VRN"
        H.Load from -> shortLabel from
        H.External src _ -> shortLabel src


instance ShortChannelLabel H.Url where
    shortLabel :: H.Url -> VShortChannelLabel
    shortLabel = const "🔗" -- '#'


instance ShortChannelLabel H.GlslFn where
    shortLabel :: H.GlslFn -> VShortChannelLabel
    shortLabel = const "↬"


instance ShortChannelLabel H.SourceOptions where
    shortLabel :: H.SourceOptions -> VShortChannelLabel
    shortLabel = const "OPT"


instance ShortChannelLabel H.Values where
    shortLabel :: H.Values -> VShortChannelLabel
    shortLabel (H.Values array) = "[" <> (show $ Array.length array) <> "]"


instance ShortChannelLabel H.Ease where
    shortLabel :: H.Ease -> VShortChannelLabel
    shortLabel = case _ of
        H.Linear -> "╱"
        H.Fast _ -> "⏭"
        H.Smooth _ -> "↝"
        H.Fit _ -> "⇔"
        H.Offset _ -> "⏼"
        H.InOutCubic -> "⊰"


instance ShortChannelLabel H.AudioSource where
    shortLabel :: H.AudioSource -> VShortChannelLabel
    shortLabel = case _ of
        H.Silence -> "⏹" -- 🔇
        H.Mic -> "" -- TODO
        H.File -> "" -- TODO


instance ShortChannelLabel H.AudioBin where
    shortLabel :: H.AudioBin -> VShortChannelLabel
    shortLabel (H.AudioBin n) = "H" <> show n -- 🔈 🔉


instance ShortChannelLabel H.OutputN where
    shortLabel :: H.OutputN -> VShortChannelLabel
    shortLabel = case _ of
        H.Output0 -> "⎑0" -- OUT0
        H.Output1 -> "⎑1" -- OUT1
        H.Output2 -> "⎑2" -- OUT2
        H.Output3 -> "⎑3" -- OUT3
        H.Output4 -> "⎑4" -- OUT4


instance ShortChannelLabel H.SourceN where
    shortLabel :: H.SourceN -> VShortChannelLabel
    shortLabel = case _ of
        H.Source0 -> "S0"


instance ShortChannelLabel H.RenderTarget where
    shortLabel :: H.RenderTarget -> VShortChannelLabel
    shortLabel = case _ of
        H.Four -> "∀"
        H.Output oN -> shortLabel oN


instance ShortChannelLabel H.Fn where
    shortLabel :: H.Fn -> VShortChannelLabel
    shortLabel = case _ of
        H.UserExpr jsexpr -> "EXP"
        H.Fn code -> "PS"
        H.Unparsed str -> "STR"
        H.NoAction -> "--"


instance ShortChannelLabel H.CanBeSource where
    shortLabel :: H.CanBeSource -> VShortChannelLabel
    shortLabel (H.CanBeSource cbs) = case cbs of
        Left sourceN -> shortLabel sourceN
        Right outputN -> shortLabel outputN


instance ShortChannelLabel H.TOrV where
    shortLabel :: H.TOrV -> VShortChannelLabel
    shortLabel (H.T tex) = shortLabel tex
    shortLabel (H.V val) = shortLabel val


instance ShortChannelLabel CAI.Products where
    shortLabel :: CAI.Products -> VShortChannelLabel
    shortLabel ps = show (CAI.count ps) <> "P"


instance ShortChannelLabel CAI.Product' where
    shortLabel :: CAI.Product' -> VShortChannelLabel
    shortLabel = CAI.toShortId'


-- TODO: use color tags

instance StatusLineInfo H.Value where
    statusLine :: H.Value -> VStatusLine
    statusLine = show


instance StatusLineInfo Unit where
    statusLine :: Unit -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Texture where
    statusLine :: H.Texture -> VStatusLine
    statusLine = show


instance StatusLineInfo H.TODO where
    statusLine :: H.TODO -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Context where
    statusLine :: H.Context -> VStatusLine
    statusLine = show


instance StatusLineInfo H.UpdateFn where
    statusLine :: H.UpdateFn -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Source where
    statusLine :: H.Source -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Url where
    statusLine :: H.Url -> VStatusLine
    statusLine = show


instance StatusLineInfo H.GlslFn where
    statusLine :: H.GlslFn -> VStatusLine
    statusLine = show


instance StatusLineInfo H.SourceOptions where
    statusLine :: H.SourceOptions -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Values where
    statusLine :: H.Values -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Ease where
    statusLine :: H.Ease -> VStatusLine
    statusLine = show


instance StatusLineInfo H.AudioSource where
    statusLine :: H.AudioSource -> VStatusLine
    statusLine = show


instance StatusLineInfo H.AudioBin where
    statusLine :: H.AudioBin -> VStatusLine
    statusLine = show


instance StatusLineInfo H.OutputN where
    statusLine :: H.OutputN -> VStatusLine
    statusLine = show


instance StatusLineInfo H.SourceN where
    statusLine :: H.SourceN -> VStatusLine
    statusLine = show


instance StatusLineInfo H.ExtSource where
    statusLine :: H.ExtSource -> VStatusLine
    statusLine = show


instance StatusLineInfo H.Fn where
    statusLine :: H.Fn -> VStatusLine
    statusLine = show


instance StatusLineInfo H.RenderTarget where
    statusLine :: H.RenderTarget -> VStatusLine
    statusLine = show


instance StatusLineInfo H.CanBeSource where
    statusLine :: H.CanBeSource -> VStatusLine
    statusLine = show


instance StatusLineInfo CAI.Products where
    statusLine :: CAI.Products -> VStatusLine
    statusLine = show


instance StatusLineInfo CAI.Product' where
    statusLine :: CAI.Product' -> VStatusLine
    statusLine = show


instance StatusLineInfo H.TOrV where
    statusLine :: H.TOrV -> VStatusLine
    statusLine (H.T tex) = statusLine tex
    statusLine (H.V val) = statusLine val


instance (ShortChannelLabel a, StatusLineInfo a) => NMF.HasRepr a InfoRepr where
    toRepr :: forall f i o. InNode f i o -> a -> InfoRepr
    toRepr _ a = InfoRepr { shortLabel : shortLabel a, statusLine : statusLine a }


instance (ShortChannelLabel a, StatusLineInfo a) => R.ToRepr a InfoRepr where
    toRepr :: a -> Maybe (R.Repr InfoRepr)
    toRepr = R.exists <<< InfoRepr <<< \a -> { shortLabel : shortLabel a, statusLine : statusLine a }


instance ShortChannelLabel W.WrapRepr where
    shortLabel :: W.WrapRepr -> VShortChannelLabel
    shortLabel = case _ of
        W.Value v -> shortLabel v
        W.Unit u -> shortLabel u
        W.Texture tex -> shortLabel tex
        W.TOrV (H.T t) -> shortLabel t
        W.TOrV (H.V v) -> shortLabel v
        W.OutputN on -> shortLabel on
        W.SourceN sn -> shortLabel sn
        W.TODO todo -> shortLabel todo
        W.Context ctx -> shortLabel ctx
        W.UpdateFn fn -> shortLabel fn
        W.Source src -> shortLabel src
        W.Url url -> shortLabel url
        W.GlslFn glsl -> shortLabel glsl
        W.SourceOptions opts -> shortLabel opts
        W.Values vals -> shortLabel vals
        W.Ease ease -> shortLabel ease
        W.Audio audio -> shortLabel audio
        W.AudioBin bin -> shortLabel bin
        W.ExtSource ext -> shortLabel ext
        W.Fn fn -> shortLabel fn
        W.Target trg -> shortLabel trg
        W.Products products -> shortLabel products
        W.Product product -> shortLabel product
        W.CBS cbs -> shortLabel cbs


instance StatusLineInfo W.WrapRepr where
    statusLine :: W.WrapRepr -> VStatusLine
    statusLine = case _ of
        W.Value v -> statusLine v
        W.Unit u -> statusLine u
        W.Texture tex -> statusLine tex
        W.TOrV (H.T t) -> statusLine t
        W.TOrV (H.V v) -> statusLine v
        W.OutputN on -> statusLine on
        W.SourceN sn -> statusLine sn
        W.TODO todo -> statusLine todo
        W.Context ctx -> statusLine ctx
        W.UpdateFn fn -> statusLine fn
        W.Source src -> statusLine src
        W.Url url -> statusLine url
        W.GlslFn glsl -> statusLine glsl
        W.SourceOptions opts -> statusLine opts
        W.Values vals -> statusLine vals
        W.Ease ease -> statusLine ease
        W.Audio audio -> statusLine audio
        W.AudioBin bin -> statusLine bin
        W.ExtSource ext -> statusLine ext
        W.Fn fn -> statusLine fn
        W.Target trg -> statusLine trg
        W.Products products -> statusLine products
        W.Product product -> statusLine product
        W.CBS cbs -> statusLine cbs



instance Documentation (Family "osc") where
    docs = const $ familyDocs "osc"


-- instance Documentation FamilyR where
--     docs (FamilyR "osc") = docs (Family :: _ "osc")


instance Documentation FamilyR where
    docs = familyDocs <<< reflect'


familyDocs :: String -> String
familyDocs = case _ of -- use ToFn

    -- Feed
    "number" -> "Create a number value to pass further"
    "pi" -> "Use PI value, which is close to ~3.14 to pass further"
    "array" -> "Create an array of number values to pass it further"
    "expression" -> "Define a JavaScript expression to be called on every frame" -- TODO: width * Math.PI, mouse.x * 2, (1.05 + 0.1 * Math.sin(0.05*time)) , a.fft[1]*40... JsExprParsing
    "callFunction" -> ""

    "info" -> "Show both quick information and documentation for the hovered subject"

    -- Source
    "noise" -> "Smooth sequences of random values over 2D plane in grayscale"
    "voronoi" -> "The net of triangles of different sizes, in grayscale"
    "osc" -> "The horizontal wave, oscillating between black and white"
    "shape" -> "A polygon with given number of vertices, the larger the number, the more roundier is the shape"
    "gradient" -> "RGB gradient through all colors, in 2D"
    "src" -> "Take some output (see `out`) by number and make it a source for another sequence of transformations"
    "solid" -> "Just the given color"
    "prev" -> "--"

    -- Geometry
    "rotate" -> "Rotate the texture by given degree"
    "scale" -> "Scale the texture by given amount"
    "pixelate" -> "Break the texture into the solid color blocks of given size"
    "repeat" -> "Repeat the texture by X and Y given amount of times"
    "repeatX" -> "Repeat the texture by X given amount of times"
    "repeatY" -> "Repeat the texture by Y given amount of times"
    "kaleid" -> "Radially mirror the texture around the center given number of times"
    "scroll" -> "Move the texture by X and Y with given speed"
    "scrollX" -> "Move the texture by X with given speed"
    "scrollY" -> "Move the texture by Y with given speed"

    -- Color
    "posterize" -> "Reduce the number of colors on the texture to a given amount, makes the palette of the texture sparser"
    "shift" -> ""
    "invert" -> "Invert the colors on the texture"
    "contrast" -> "Raise the contrast on the texture"
    "brightness" -> "Make the texture brighter"
    "luma" -> ""
    "thresh" -> "Set a treshold on the colors on the texture"
    "color" -> ""
    "saturate" -> "Adjust the saturation level of colors on the texture"
    "hue" -> "Adjust the hue of colors on the texture"
    "colorama" -> ""
    "sum" -> ""
    "r" -> "Take only red part of the colors on the texture"
    "g" -> "Take only green part of the colors on the texture"
    "b" -> "Take only blue part of the colors on the texture"
    "a" -> "Take only alpha (opacity) part of the colors on the texture"

    -- Blend
    "add" -> "Add the colors on both textures to a resulting texture"
    "sub" -> "Subtract the colors on both textures to a resulting texture"
    "layer" -> "Overlay one texture over another"
    "blend" -> "Blend the colors on both textures with each other to get a resulting texture"
    "mult" -> "Multiply the colors on two texture on each other"
    "diff" -> "Produce a texture from the difference between the colors of two textures"
    "mask" -> "Mask one texture with another"

    -- Modulate
    "modulateRepeat" -> "Use another texture's red channel values as the source for repeating"
    "modulateRepeatX" -> "Use another texture's red channel values as the source for repeating by X axis"
    "modulateRepeatY" -> "Use another texture's red channel values as the source for repeating by Y axis"
    "modulateKaleid" -> "Use another texture's red channel values as the source for mirroring the image"
    "modulateScrollX" -> "Use another texture's red channel values as the source for scrolling"
    "modulateScrollY" -> "Use another texture's red channel values as the source for scrolling by X axis"
    "modulate" -> "Use another texture's red channel values as the source for translating pixels of this one"
    "modulateScale" -> "Use another texture's red channel values as the source for scaling"
    "modulatePixelate" -> "Use another texture's red channel values as the source for pixelating"
    "modulateRotate" -> "Use another texture's red channel values as the source for rotation"
    "modulateHue" -> "Use another texture's pixel values as the source for changing hue"

    -- ExternalSources
    "initCam" -> "Select and initialize the camera to be used (use `src`)"
    "initImage" -> "Select the image source to be used (use `src`)"
    "initVideo" -> "Select the video source to be used (use `src`)"
    "init" -> "Set up the properties of the scene"
    "initStream" -> "Select the video stream source to be used (use `src`)"
    "initScreen" -> "Select the screen to be integrated in the scene (use `src`)"

    -- Synth
    "render" -> "Render all (four) outputs simultaneously"
    "update" -> "Specify the function to be called on every frame"
    "setResolution" -> "Set the sketch size manually"
    "hush" -> ""
    "setFunction" -> "Define a custom GLSL function to be used as a texture source or a modifier"
    "speed" -> "Set the speed of the scene"
    "bpm" -> "Set the audio BPM for the sceen"
    "width" -> "Get current width of the screen"
    "height" -> "Get current height of the screen"
    "time" -> "Get current time"
    "mouse" -> "Get current mouse position"

    -- Audio
    "fft" -> "Get the harmonic of the audio by a number of a bin (see `setBins`)"
    "setSmooth" -> "Set how smooth should be the discretion of audio values"
    "setCutoff" -> "Set minimum cutoff value for audio"
    "setBins" -> "Set number of bins in the audio analysis"
    "setScale" -> "Set the proportion to scale the audio levels to"
    "hide" -> ""
    "show" -> ""

    -- Array
    "fast" -> "Specify how fast should we switch between values in the array"
    "smooth" -> "Make switching between values in the array smooth"
    "ease" -> "Select the easing on switching between the values in the array"
    "offset" -> ""
    "fit" -> ""

    -- Out
    "out" -> "Send the texture to the given output (one of four, `0` by default)"

    _ -> ""


    -- "colorama" -> "Shift HSV values"
    -- _ -> "tralala"