module HydraTk.Lang.Program where

import Prelude

import Prelude (class Show, show) as Core

import Effect.Class (class MonadEffect)

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe(Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map
import Data.List as List
import Data.Array ((:))
import Data.Array (length, fromFoldable) as Array
import Data.Foldable (foldr)
import Data.Traversable (sequence)
import Data.String as String

import Noodle.Id as Id
-- import Noodle.Node.MapsFolds.Flatten as R

import Noodle.Fn.Signature as Sig
import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.Code.Target (PS, JS, JS_DISPLAY, pureScript, javaScript, javaScriptToDisplay)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, state, NodeChanges) as RawNode
import Noodle.Patch (Patch)
import Noodle.Patch (mapAllNodes) as Patch
import Noodle.Repr.ValueInChannel (toMaybe) as ViC

import HydraTk.Types as H
import HydraTk.Lang.Command (Command(..))
import HydraTk.Repr.Wrap (WrapRepr(..))
import HydraTk.Repr.State (StateRepr(..))
import HydraTk.Repr.Show as H
-- import Toolkit.Hydra.Lang.Glsl as Glsl



data Program =
    Program (Array Command)


class ToHydraCommand a where -- FIXME: temporary typeclass while we testing Hydra universally, remove it after
    toHydraCommand :: Id.FamilyR -> a -> Maybe Command


instance ToHydraCommand StateRepr where
    toHydraCommand familyR outValue =
        case Id.family familyR /\ outValue of
            "out" /\ Cmd command -> Just command
            _ -> Nothing


empty :: Program
empty = Program []


printToJavaScript :: Program -> String
printToJavaScript = toCode javaScript unit


instance Show a => Show Program where
  show :: Program -> String
  show (Program cmds) = show cmds


instance ToCode PS opts Program where
    toCode _ opts (Program cmds) = "PROGRAM" -- IMPLEMENT toCode pureScript opts cmd
else instance ToCode JS opts Program where
    toCode _ opts prg@(Program cmds) =
        let glslUsage = [] -- IMPLEMENT collectGlslUsage prg
        in
        "/* GENERATED CODE */\n\n" <>
        ( if Array.length glslUsage > 0
            then "IMPLEMENT" -- String.joinWith "\n\n" (toCode javaScript opts <$> collectGlslUsage prg) <> "\n\n"
            else ""
        )
        <> String.joinWith "\n\n" (toCode javaScript opts <$> cmds)
else instance ToCode JS_DISPLAY opts Program where
    toCode _ opts prg@(Program cmds) =
        let glslUsage = [] -- IMPLEMENT collectGlslUsage prg
        in
        ( if Array.length glslUsage > 0
            then "IMPLEMENT" -- String.joinWith "\n\n" (toCode javaScriptToDisplay opts <$> collectGlslUsage prg) <> "\n\n"
            else ""
        )
        <> String.joinWith "\n\n" (toCode javaScript opts <$> cmds)


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



formProgram :: Map Id.NodeR Command -> Program
formProgram = Map.values >>> Array.fromFoldable >>> Program


{-
commandOf :: forall a. Program a -> Command
commandOf (Program cmd _) = cmd
-}


collectHydraCommands :: forall ps fs sr cr m. MonadEffect m => ToHydraCommand sr => Patch ps fs sr cr m -> m (Map Id.NodeR Command)
collectHydraCommands =
    map Map.catMaybes <<< sequence <<< Map.fromFoldable <<< Patch.mapAllNodes toCommandTuple
    where
        toCommandTuple :: Raw.Node sr cr m -> Id.NodeR /\ m (Maybe Command)
        toCommandTuple rawNode = RawNode.id rawNode /\ extractCommandFromNode rawNode
        extractCommandFromNode :: Raw.Node sr cr m -> m (Maybe Command)
        extractCommandFromNode rawNode =
            let familyR = Id.familyOf $ RawNode.id rawNode
            in
                if producesCode familyR then
                    RawNode.state rawNode <#> toHydraCommand familyR
                else
                    pure Nothing