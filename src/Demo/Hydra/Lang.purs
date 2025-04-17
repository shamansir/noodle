module HydraTk.Lang where


import Prelude
import Prelude (class Show, show) as Core

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe(Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map
import Data.List as List
import Data.Array ((:))
import Data.Array (length) as Array
import Data.Foldable (foldr)
import Data.String as String

import Noodle.Id as Id
-- import Noodle.Node.MapsFolds.Flatten as R

import Noodle.Fn.Signature as Sig
import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.Code.Target (PS, JS, JS_DISPLAY, pureScript, javaScript, javaScriptToDisplay)
import Noodle.Raw.Node (NodeChanges) as RawNode

import HydraTk.Types as H
import HydraTk.Repr.Wrap (WrapRepr(..))
import HydraTk.Repr.Show as H
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


data Program =
    Program (Array Command)


empty :: Program
empty = Program []


instance Show a => Show Program where
  show :: Program -> String
  show (Program cmds) = show cmds


instance ToCode PS opts Command where
    toCode _ opts  = case _ of
        Unknown -> "{- unknown -}"
        Single _ -> "{- single -}"
        Chain _ _ -> "{- chain -}"

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
        where
            quickPurs :: forall a. ToCode PS opts a => a -> String
            quickPurs = toCode pureScript opts
else instance ToCode JS opts Command where
    toCode _ opts = case _ of
        Unknown -> "/* unknown */"
        Single _ -> "/* single */"
        Chain _ _ -> "/* chain */"


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
        where
            quickJs :: forall a. ToCode JS opts a => a -> String
            quickJs = toCode javaScript opts
        -}


{-
collectGlslUsage :: forall a. Program a -> Array H.GlslFn
collectGlslUsage prg = fold checkCmdForRefs [] prg
    where
        checkCmdForRefs (Continue tex) arr = checkTexForRefs tex <> arr
        checkCmdForRefs (End _ tex) arr = checkTexForRefs tex <> arr
        checkCmdForRefs _ arr = arr
        checkTexForRefs (H.BlendOf { with, what } _) = checkTexForRefs with <> checkTexForRefs what
        checkTexForRefs (H.Filter tex _) = checkTexForRefs tex
        checkTexForRefs (H.ModulateWith { with, what } _) = checkTexForRefs with <> checkTexForRefs what
        checkTexForRefs (H.Geometry tex _) = checkTexForRefs tex
        checkTexForRefs (H.CallGlslFn { over, mbWith } fnRef) = addIfJust (fnRefToGlslFn fnRef) $ checkTexForRefs over <> case mbWith of
            Just with -> checkTexForRefs with
            Nothing -> mempty
        checkTexForRefs _ = []
        fnRefToGlslFn (H.GlslFnRef fn) = Nothing -- FIXME: Map.lookup (Sig.name fn) Glsl.knownFnsMap
        addIfJust :: _
        addIfJust (Just glslFn) arr = glslFn : arr
        addIfJust Nothing arr = arr
-}


{-
instance ToCode PS opts (Program a) where
    toCode _ opts (Program cmd _) = toCode pureScript opts cmd
else instance ToCode JS opts (Program a) where
    toCode _ opts prg@(Program cmd _) =
        let glslUsage = collectGlslUsage prg
        in
        "/* GENERATED CODE */\n\n" <>
        ( if Array.length glslUsage > 0
            then "IMPLEMENT" -- String.joinWith "\n\n" (toCode javaScript opts <$> collectGlslUsage prg) <> "\n\n"
            else ""
        )
        <> toCode javaScript opts cmd
else instance ToCode JS_DISPLAY opts (Program a) where
    toCode _ opts prg@(Program cmd _) =
        let glslUsage = collectGlslUsage prg
        in
        ( if Array.length glslUsage > 0
            then "IMPLEMENT" -- String.joinWith "\n\n" (toCode javaScriptToDisplay opts <$> collectGlslUsage prg) <> "\n\n"
            else ""
        )
        <> toCode javaScript opts cmd
-}


producesCode :: Id.FamilyR -> Boolean
producesCode familyR = case Id.family familyR of
        "out" -> true

        "initCam" -> true
        "initImage" -> true
        "initVideo" -> true
        "init" -> true
        "initScreen" -> true
        "initStream" -> true

        "speed" -> true
        "bpm" -> true
        "setResolution" -> true
        "update" -> true
        "render" -> true

        -- "callFunction" -> true
        -- "caiGradientShader" -> true

        _ -> false


codeOrder :: Id.FamilyR -> Int
codeOrder familyR = case Id.family familyR of
        "speed" -> 0
        "bpm" -> 0
        "setResolution" -> 0
        "update" -> 0

        "initCam" -> 1
        "initImage" -> 1
        "initVideo" -> 1
        "init" -> 1
        "initScreen" -> 1
        "initStream" -> 1

        "out" -> 2

        "render" -> 3

        _ -> 4


{- IMPLEMENT -}
{-
changesToCommand :: forall strepr. Id.FamilyR -> RawNode.NodeChanges strepr WrapRepr -> Command -- Int /\ Command?
changesToCommand familyR update =
    case Id.family familyR of

        "out" ->
            -- fn2 "what" "target" $ \v -> case Debug.spy "_" v of
            fn2 "what" "target" $ case _ of
                Just (Texture texture /\ OutputN target) ->
                    End target texture
                _ -> Unknown

        "render" ->
            fn1 "what" $ case _ of
                Just (Target target) ->
                    One $ Render target
                _ -> Unknown

        "speed" ->
            fn1 "v" $ case _ of
                Just (Value val) ->
                    One $ Speed val
                _ -> Unknown

        "bpm" ->
            fn1 "v" $ case _ of
                Just (Value val) ->
                    One $ Bpm val
                _ -> Unknown

        "hush" ->
            One Hush

        "update" ->
            Unknown
            -- FIXME

        "setFunction" ->
            Unknown
            -- FIXME

        "init" ->
            fn1 "options" $ case _ of
                Just (SourceOptions so) ->
                    One $ Init so
                _ -> Unknown

        "initImage" ->
            -- fn2 "src" "url" $ case _ of
            -- eiher src or url
            -- FIXME
            Unknown

        "initCam" ->
            fn2 "src" "index" $ case _ of -- FIXME index should be optional
                Just (Source src /\ Value index) ->
                    One $ InitCamIdx src index
                _ -> fn1 "src" $ case _ of
                        Just (Source src) ->
                            One $ InitCam src
                        _ -> Unknown

        "initVideo" ->
            -- fn2 "src" "url" $ case _ of
            -- eiher src or url
            -- FIXME
            Unknown

        "initScreen" ->
            fn1 "options" $ case _ of
                Just (Source src) ->
                    One $ InitScreen src
                _ -> Unknown

        "initStream" ->
            -- fn1 "src" $ case _ of
            -- TODO
            Unknown

        "setResolution" ->
            fn2 "width" "height" $ case _ of
                Just (Value width /\ Value height) ->
                    One $ SetResolution width height
                _ -> Unknown



        _ -> Unknown
    where
        fn1 i0 cnv =
            cnv $ Map.lookupBy' ?wn i0 update.inlets
        fn2 i0 i1 cnv =
            cnv $ (/\) <$> Map.lookupBy' ?wh i0 update.inlets <*> Map.lookupBy' ?wh i1 update.inlets
-}


{-
formProgram :: Map Id.NodeR Command -> Program
formProgram = Map.values >>> List.foldl append empty
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


{-
commandOf :: forall a. Program a -> Command
commandOf (Program cmd _) = cmd
-}