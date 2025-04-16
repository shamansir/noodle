module Demo.Hydra.Lang where


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
-- import Toolkit.Hydra.Lang.Glsl as Glsl


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
        End output texture -> "IMPLEMENT" -- ". " <> show output <> " <> " <> show texture
        Pair texA texB -> show texA <> " /\\ " <> show texB
        One single -> show  "1 " <> show single
        Continue tex -> "IMPLEMENT" -- "~ " <> show tex


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


instance ToCode PS opts Command where
    toCode _ opts  = case _ of
        Unknown -> "{- unknown -}"
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
        where
            quickPurs :: forall a. ToCode PS opts a => a -> String
            quickPurs = toCode pureScript opts
else instance ToCode JS opts Command where
    toCode _ opts = case _ of
        Unknown -> "/* unknown */"
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


formProgram :: Map Id.NodeR Command -> Program Unit
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