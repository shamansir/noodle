module Toolkit.Hydra2.Lang where

import Prelude
import Prelude (class Show, show) as Core

import Debug as Debug


{-
import Data.Functor (class Functor)
import Data.Semigroup (class Semigroup)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
-}

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.SProxy (reflect, reflect')
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
import Noodle.Node2.MapsFolds.Flatten as R

import Toolkit.Hydra2.Types as H
import Toolkit.Hydra2.Lang.Fn as Fn

import Toolkit.Hydra2.Lang.ToCode
import Toolkit.Hydra2.Lang.ToCode (fnPs, fnJs) as ToCode
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..))
import Toolkit.Hydra2.Lang.Glsl as Glsl


data Single
    = WithAudio H.OnAudio
    | Speed H.Value
    | Bpm H.Value
    | Hush
    | Init H.SourceOptions
    -- TODO : join under `WithSource`
    | InitCam H.Source
    | InitCamIdx H.Source H.Value
    | InitScreen H.Source
    | Clear H.Source
    | SetResolution H.Value H.Value
    | Render H.RenderTarget


data Command
    = Unknown
    | End H.OutputN H.Texture
    | Pair Command Command -- parent -> child ?
    -- | Batch (Array Command)
    | One Single
    | Continue H.Texture


instance Show Single where
    show :: Single -> String
    show = case _ of
        _ -> "TODO / FIXME"
        -- WithAudio n -> "withAudio " <> show n


instance Show Command where
    show :: Command -> String
    show = case _ of
        Unknown -> "Unknown"
        End output texture -> ". " <> show output <> " <> " <> show texture
        Pair texA texB -> show texA <> " /\\ " <> show texB
        One single -> show  "1 " <> show single
        Continue tex -> "~ " <> show tex


data Program a = -- same as Writer?
    Program Command a


instance Semigroup Command where
    append = Pair


empty :: Program Unit
empty = Program Unknown unit


append :: Program Unit -> Command -> Program Unit
append (Program Unknown _) Unknown = Program Unknown unit
append (Program Unknown _) cmd = Program cmd unit
append (Program cmd _) Unknown = Program cmd unit
append (Program cmdA _) cmdB = Program (Pair cmdA cmdB) unit


instance Show a => Show (Program a) where
  show :: Program a -> String
  show (Program cmd a) = show a <> " <- " <> show cmd


unfold :: forall a. Program a -> Array Command
unfold (Program cmd _) =
    unfoldCmd cmd
    where
        unfoldCmd = case _ of
            Pair cmdA cmdB -> Pair cmdA cmdB : (unfoldCmd cmdA <> unfoldCmd cmdB)
            otherCmd -> [ otherCmd ]


fold :: forall a b. (Command -> b -> b) -> b -> Program a -> b
fold f b = foldr f b <<< unfold


-- instance Semigroup (Program Unit) where
--     append :: Program Unit -> Program Unit -> Program Unit
--     append (Program cmdA _) (Program cmdB _) =
--         Program ( cmdA <> cmdB ) unit


instance Semigroup a => Semigroup (Program a) where
    append :: Program a -> Program a -> Program a
    append (Program cmdA a) (Program cmdB b) =
        Program ( cmdA <> cmdB ) (a <> b)


instance Functor Program where
    map :: forall a b. (a -> b) -> Program a -> Program b
    map fa (Program aitems a) =
        Program aitems $ fa a


instance Apply Program where
    apply :: forall a b. Program (a -> b) -> Program a -> Program b
    apply (Program fitems fa) (Program aitems a) =
        Program (fitems <> aitems) $ fa a


instance Bind Program where
    bind :: forall a b. Program a -> (a -> Program b) -> Program b
    bind (Program aitems aval) f =
        case f aval of
            Program bitems b -> Program (aitems <> bitems) b


instance ToCode PS Command where
    toCode _ = case _ of
        Unknown -> "{- unknown -}"
        End output texture -> toCode pureScript texture <> "\n\t# " <> ToCode.fnPs "out" [ output ]
        Pair cmdA cmdB -> toCode pureScript cmdA <> "\n" <> toCode pureScript cmdB
        One (WithAudio onaudio) -> toCode pureScript onaudio
        One (Render H.Four) -> "renderAll"
        One (Render (H.Output outN)) -> toCode pureScript outN <> " # render"
        One (Speed val) -> toCode pureScript val <> " # speed"
        One (Bpm val) -> toCode pureScript val <> " # bpm"
        One Hush -> "hush"
        One (Init so) -> "init" -- FIXME: toCode pureScript so <> " # init"
        One (InitScreen src) -> toCode pureScript src <> " # initScreen"
        One (InitCam src) -> toCode pureScript src <> " # initCam"
        One (InitCamIdx src index) -> toCode pureScript src <> " # initCamIdx " <> toCode pureScript index
        One (Clear src) -> toCode pureScript src <> " # clear"
        One (SetResolution width height) -> "{ width : " <> toCode pureScript width <> ", height : " <> toCode pureScript height <> " # setResolution"
        Continue texture -> "." <> toCode pureScript texture
else instance ToCode JS Command where
    toCode _ = case _ of
        Unknown -> "/* unknown */"
        End output texture ->
            -- case Debug.spy "tex" texture of
            case texture of
                H.Empty -> ""
                _ -> toCode javaScript texture <> "\n\t." <> ToCode.fnJs "out" [ output ]
        Pair cmdA cmdB -> toCode javaScript cmdA <> "\n" <> toCode javaScript cmdB
        One (WithAudio onaudio) -> toCode javaScript onaudio
        -- One (InitCam src index) -> toCode javaScript src <> ".initCam( " <> toCode javaScript index <> " )"
        One (Render H.Four) -> "render()"
        One (Render (H.Output outN)) -> toCode javaScript outN <> ".render()"
        One (Speed val) -> "speed = " <> toCode javaScript val
        One (Bpm val) -> "bpm = " <> toCode javaScript val
        One Hush -> "hush()"
        One (Init so) -> "init" -- FIXME: toCode javaScript so <> " # init"
        One (InitScreen src) -> "initScreen( " <> toCode javaScript src <> " )"
        One (InitCam src) -> "initCam( " <> toCode javaScript src <> " )"
        One (InitCamIdx src index) -> "initCam( " <> toCode javaScript src <> " , " <> toCode javaScript index <> " )"
        One (Clear so) -> toCode javaScript so <> ".clear()"
        One (SetResolution width height) -> "setResolution( " <> toCode javaScript width <> " , " <> toCode javaScript height <> " )"
        Continue texture -> "." <> toCode javaScript texture


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
        fnRefToGlslFn (H.GlslFnRef fn) = Map.lookup (Fn.name fn) Glsl.knownFnsMap
        addIfJust :: _
        addIfJust (Just glslFn) arr = glslFn : arr
        addIfJust Nothing arr = arr


instance ToCode PS (Program a) where
    toCode _ (Program cmd _) = toCode pureScript cmd
else instance ToCode JS (Program a) where
    toCode _ prg@(Program cmd _) =
        let glslUsage = collectGlslUsage prg
        in
        "/* GENERATED CODE */\n\n" <>
        ( if Array.length glslUsage > 0
            then String.joinWith "\n\n" (toCode javaScript <$> collectGlslUsage prg) <> "\n\n"
            else ""
        )
        <> toCode javaScript cmd
else instance ToCode JS_DISPLAY (Program a) where
    toCode _ prg@(Program cmd _) =
        let glslUsage = collectGlslUsage prg
        in
        ( if Array.length glslUsage > 0
            then String.joinWith "\n\n" (toCode javaScriptToDisplay <$> collectGlslUsage prg) <> "\n\n"
            else ""
        )
        <> toCode javaScript cmd



producesCode :: forall f. IsSymbol f => Id.Family f -> Boolean
producesCode family = case reflect family of
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


codeOrder :: forall f. IsSymbol f => Id.Family f -> Int
codeOrder family = case reflect family of
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



updateToCommand :: forall f. IsSymbol f => Id.Family f -> R.NodeLineMap WrapRepr -> Command -- Int /\ Command?
updateToCommand family (nodeId /\ _ /\ inputs /\ outputs) =
    case reflect family of

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
            cnv $ Map.lookupBy' reflect' i0 inputs
        fn2 i0 i1 cnv =
            cnv $ (/\) <$> Map.lookupBy' reflect' i0 inputs <*> Map.lookupBy' reflect' i1 inputs


formProgram :: Map Id.NodeIdR Command -> Program Unit
formProgram = Map.values >>> List.foldl append empty


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