module HydraTk.Lang.Command where

import Prelude

import Data.Tuple.Nested ((/\))


import Noodle.Fn.Signature as Sig
import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.Code.Target (PS, JS, JS_DISPLAY, pureScript, javaScript, javaScriptToDisplay)

import HydraTk.Types as H
import HydraTk.Repr.Show (hShow) as H
-- import Toolkit.Hydra.Lang.Glsl as Glsl


data Single
    = WithAudio H.AudioSource H.OnAudio
    | WithSynth H.OnSynth
    | WithSource H.SourceN H.OnSource
    | SynthSet H.SynthProp


data Command
    = Unknown
    | Single Single
    | Chain H.OutputN H.Texture


instance Show Single where
    show :: Single -> String
    show = case _ of
        WithAudio src _ -> "withAudio " <> H.hShow src
        WithSynth syn -> "withSynth " -- <> H.hShow syn
        WithSource src syn -> "withSrc " <> H.hShow src <> " " -- <> H.hShow syn
        SynthSet prop -> "synthSet"
        -- WithAudio n -> "withAudio " <> show n


instance Show Command where
    show :: Command -> String
    show = case _ of
        Unknown -> "Unknown"
        Chain output texture -> H.hShow output <> " <> " <> H.hShow texture
        Single single -> show  "1 " <> show single


instance ToCode PS opts Command where
    toCode _ opts  = case _ of
        Unknown -> "{- unknown -}"
        Single _ -> "{- single -}"
        Chain outputN texture -> H.textureToPureScript texture <> "\n\t# " <> Sig.toPureScript (Sig.sig1 "out" $ "output" /\ outputN)

        {- }
        End output texture -> "IMPLEMENT" -- quickPurs texture <> "\n\t# " <> Sig.toPureScript' (Sig.sig1 "out" $ "output" /\ output)
        Pair cmdA cmdB -> quickPurs cmdA <> "\n" <> quickPurs cmdB
        One (WithAudio onaudio) -> "IMPLEMENT" -- quickPurs onaudio
        One (Render H.Four) -> "renderAll"
        One (Render (H.Output outN)) -> "IMPLEMENT" -- quickPurs outN <> " # render"
        One (Speed val) -> "IMPLEMENT" -- quickPurs val <> " # speed"
        One (Bpm val) -> "IMPLEMENT" -- quickPurs val <> " # bpm"
        One Hush -> "hush"
        One (Init so) -> "init" -- FIXME: quickPurs so <> " # init"
        One (InitScreen src) -> "IMPLEMENT" -- quickPurs src <> " # initScreen"
        One (InitCam src) -> "IMPLEMENT" -- quickPurs src <> " # initCam"
        One (InitCamIdx src index) -> "IMPLEMENT" -- quickPurs src <> " # initCamIdx " <> quickPurs index
        One (Clear src) -> "IMPLEMENT" -- quickPurs src <> " # clear"
        One (SetResolution width height) -> "IMPLEMENT" -- "{ width : " <> quickPurs width <> ", height : " <> quickPurs height <> " # setResolution"
        Continue texture -> "IMPLEMENT" -- "." <> quickPurs texture
        -}
else instance ToCode JS opts Command where
    toCode _ opts = case _ of
        Unknown -> "/* unknown */"
        Single _ -> "/* single */"
        Chain outputN texture ->
            case texture of
                H.Empty -> ""
                _ -> H.textureToJavaScript texture <> "\n\t# " <> Sig.toJavaScript (Sig.sig1 "out" $ "output" /\ outputN)
        {-
        End output texture ->
            -- case Debug.spy "tex" texture of
            case texture of
                H.Empty -> ""
                _ -> "IMPLEMENT" -- quickJs texture <> "\n\t." <> Sig.toJavaScript' (Sig.sig1 "out" $ "output" /\ output)
        Pair cmdA cmdB -> quickJs cmdA <> "\n" <> quickJs cmdB
        One (WithAudio onaudio) -> "IMPLEMENT" -- quickJs onaudio
        -- One (InitCam src index) -> quickJs src <> ".initCam( " <> quickJs index <> " )"
        One (Render H.Four) -> "render()"
        One (Render (H.Output outN)) -> "IMPLEMENT" -- quickJs outN <> ".render()"
        One (Speed val) -> "IMPLEMENT" -- "speed = " <> quickJs val
        One (Bpm val) -> "IMPLEMENT" -- "bpm = " <> quickJs val
        One Hush -> "hush()"
        One (Init so) -> "init" -- FIXME: quickJs so <> " # init"
        One (InitScreen src) -> "IMPLEMENT" -- "initScreen( " <> quickJs src <> " )"
        One (InitCam src) -> "IMPLEMENT" -- "initCam( " <> quickJs src <> " )"
        One (InitCamIdx src index) -> "IMPLEMENT" -- "initCam( " <> quickJs src <> " , " <> quickJs index <> " )"
        One (Clear so) -> "IMPLEMENT" -- quickJs so <> ".clear()"
        One (SetResolution width height) -> "IMPLEMENT" -- "setResolution( " <> quickJs width <> " , " <> quickJs height <> " )"
        Continue texture -> "IMPLEMENT" -- "." <> quickJs texture
        -}


{-
instance Core.Show Texture where
    show (Filter _ c) = "color <- " <> Core.show c
    show (ModulateWith ca _ cb) = "mod <- " <> Core.show ca <> " <- " <> Core.show cb
    show (Geometry _ c) = "geom <- " <> Core.show c
    show (BlendOf ca _ cb) = "blend <- " <> Core.show ca <> " <- " <> Core.show cb
    show (From _) = "src"
    show None = "none"
-}


{-}
instance Core.Show Command where
    show Unknown = "unknown"
    show End = "end"
    show (Pair cmdA cmdB) = Core.show cmdA <> " -> " <> Core.show cmdB
    show (One (WithFrom from)) = "with from"
    show (One (WithAudio str)) = "with audio"
    show (One (InitCam str)) = "init cam"
    show (One (Render _)) = "render"
    show (Continue (Filter _)) = "cont with color"
    show (Continue (WithModulate _)) = "cont with mod"
    show (Continue (WithGeometry _)) = "cont with geom"
    show (Continue (WithSource _)) = "cont with src"
    show (To from) = "out"
-}

{-}
instance Core.Show a => Core.Show (Program a) where
    show :: Program a -> String
    show (Program items a) = Core.show items <> "    " <> Core.show a -}
