module Toolkit.Hydra2.Lang where

import Prelude
import Prelude (class Show, show) as Core


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


import Noodle.Id as Id
import Noodle.Node2.MapsFolds.Flatten as R

import Toolkit.Hydra2.Types (From(..), Output, Source, Texture, Audio, OnAudio, Value, SourceOptions) as H

import Toolkit.Hydra2.Lang.ToCode
import Toolkit.Hydra2.Lang.ToCode (fnPs, fnJs) as ToCode
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..))


data Single
    = WithAudio H.OnAudio
    | Speed H.Value
    | Bpm H.Value
    | Hush
    | Init H.SourceOptions
    | InitCam H.Source
    | InitCamIdx H.Source H.Value
    | InitScreen H.Source
    | SetResolution H.Value H.Value
    | Render H.From


data Command
    = Unknown
    | End H.Output H.Texture
    | Pair Command Command -- parent -> child ?
    -- | Batch (Array Command)
    | One Single
    | Continue H.Texture
    | To H.From -- FIXME: is it used?


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
        One (Render out) -> toCode pureScript out <> " # render"
        One (Speed val) -> toCode pureScript val <> " # speed"
        One (Bpm val) -> toCode pureScript val <> " # bpm"
        One Hush -> "hush"
        One (Init so) -> "init" -- FIXME: toCode pureScript so <> " # init"
        One (InitScreen src) -> toCode pureScript src <> " # initScreen"
        One (InitCam src) -> toCode pureScript src <> " # initCam"
        One (InitCamIdx src index) -> toCode pureScript src <> " # initCamIdx " <> toCode pureScript index
        One (SetResolution width height) -> "{ width : " <> toCode pureScript width <> ", height : " <> toCode pureScript height <> " # setResolution"
        Continue texture -> "." <> toCode pureScript texture
        To _ -> "to()"
else instance ToCode JS Command where
    toCode _ = case _ of
        Unknown -> "/* unknown */"
        End output texture -> toCode javaScript texture <> "\n\t." <> ToCode.fnJs "out" [ output ]
        Pair cmdA cmdB -> toCode javaScript cmdA <> "\n" <> toCode javaScript cmdB
        One (WithAudio onaudio) -> toCode javaScript onaudio
        -- One (InitCam src index) -> toCode javaScript src <> ".initCam( " <> toCode javaScript index <> " )"
        One (Render H.All) -> "render()"
        One (Render out) -> toCode javaScript out <> ".render()"
        One (Speed val) -> "speed = " <> toCode javaScript val
        One (Bpm val) -> "bpm = " <> toCode javaScript val
        One Hush -> "hush()"
        One (Init so) -> "init" -- FIXME: toCode javaScript so <> " # init"
        One (InitScreen src) -> "initScreen( " <> toCode javaScript src <> " )"
        One (InitCam src) -> "initCam( " <> toCode javaScript src <> " )"
        One (InitCamIdx src index) -> "initCam( " <> toCode javaScript src <> " , " <> toCode javaScript index <> " )"
        One (SetResolution width height) -> "setResolution( " <> toCode javaScript width <> " , " <> toCode javaScript height <> " )"
        Continue texture -> "." <> toCode javaScript texture
        To _ -> "to()"


instance ToCode PS (Program a) where
    toCode _ (Program cmd _) = toCode pureScript cmd
else instance ToCode JS (Program a) where
    toCode _ (Program cmd _) = toCode javaScript cmd


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
            fn2 "what" "target" $ case _ of
                Just (Texture texture /\ Output target) ->
                    End target texture
                _ -> Unknown

        "render" ->
            fn1 "from" $ case _ of
                Just (From target) ->
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
    show (WithColor _ c) = "color <- " <> Core.show c
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
    show (Continue (WithColor _)) = "cont with color"
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