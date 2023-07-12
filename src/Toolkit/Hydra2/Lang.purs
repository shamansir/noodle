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

import Toolkit.Hydra2.Types (From, Output, Source, Texture, Audio, OnAudio)

import Toolkit.Hydra2.Lang.ToCode
import Toolkit.Hydra2.Lang.ToCode (fnPs, fnJs) as ToCode
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..))


data Single
    = WithAudio OnAudio
    | InitCam Source
    | Render Output


data Command
    = Unknown
    | End Output Texture
    | Pair Command Command -- parent -> child ?
    -- | Batch (Array Command)
    | One Single
    | Continue Texture
    | To From


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
        One (InitCam src) -> toCode pureScript src <> " # initCam"
        One (Render out) -> toCode pureScript out <> " # render"
        Continue texture -> "." <> toCode pureScript texture
        To _ -> "to()"
else instance ToCode JS Command where
    toCode _ = case _ of
        Unknown -> "/* unknown */"
        End output texture -> toCode javaScript texture <> "\n\t." <> ToCode.fnJs "out" [ output ]
        Pair cmdA cmdB -> toCode javaScript cmdA <> "\n" <> toCode javaScript cmdB
        One (WithAudio onaudio) -> toCode javaScript onaudio
        One (InitCam src) -> toCode javaScript src <> ".initCam()"
        One (Render out) -> toCode javaScript out <> ".render()"
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



updateToCommand :: forall f. IsSymbol f => Id.Family f -> R.NodeLineMap WrapRepr -> Int /\ Command
updateToCommand family (nodeId /\ _ /\ inputs /\ outputs) =
    case reflect family of
        "out" ->
            2 /\
                case (/\) <$> Map.lookupBy' reflect' "what" inputs <*> Map.lookupBy' reflect' "target" inputs of
                    Just (Texture texture /\ Output target) ->
                        End target texture
                    _ -> Unknown
        _ -> 4 /\ Unknown


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