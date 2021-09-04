module Hydra.Compile where


import Prelude

import Debug as Debug

import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Array as Array
import Data.Tuple.Nested ((/\))

import Hydra
import Hydra.Fn (class ToFn, Fn(..), toFn)
import Hydra.Fn (argsToArray) as Fn


compileValue :: Value -> String
compileValue (Num n) = show n
compileValue MouseX = "() => mouse.x"
compileValue MouseY = "() => mouse.y"
compileValue Time = "() => time"
compileValue Width = "x"
compileValue Height = "y"
compileValue (Seq xs) = "[" <> (String.joinWith "," $ show <$> xs) <> "]"


compileModifier :: Modifier -> String
compileModifier mod = compileFn' mod -- compileModifier = compileFn' ???


compileFn :: forall a. ToFn a Value => a -> String
compileFn = compileFnBy compileValue <<< toFn


compileFn' :: forall a. ToFn a TextureOrValue => a -> String
compileFn' = compileFnBy (textureOrValue compileTexture compileValue) <<< toFn


compileFnBy :: forall x. (x -> String) -> Fn x -> String
compileFnBy toString (Fn { name, args })  =
    name <> "(" <> (String.joinWith "," $ compileArg <$> Fn.argsToArray args) <> ")"
    where compileArg (argName /\ value) = "/*" <> argName <> "*/ " <> toString value


compileBuffer :: Buffer -> String
compileBuffer = case _ of
    Default -> ""
    O0 -> "o0"
    O1 -> "o1"
    O2 -> "o2"
    O3 -> "o3"
    S0 -> "s0"
    S1 -> "s1"
    S2 -> "s2"
    S3 -> "s3"


compileTexture :: Texture -> String
compileTexture what =
    compile' what
    where
        compile' (Texture sourceFn modifiers) =
            compileFn sourceFn <> "\n    " <> compileModifiers modifiers
        compile' (Buffered buffer _ modifiers) =
            "src(" <> compileBuffer buffer <> ")\n    " <> compileModifiers modifiers -- FIXME: should include source
        compileModifiers modifiers = String.joinWith "\n    ." $ compileModifier <$> modifiers


compileQueue :: Queue -> String
compileQueue queue =
    String.joinWith "\n\n" $ ouputCode <$> queue
    where
        ouputCode (e /\ Default) = compileTexture e  <> "\n   .out()"
        ouputCode (e /\ buf) = compileTexture e <> "\n   .out(" <> compileBuffer buf <> ")"


compile :: Hydra -> Maybe String -- TODO: Compile only out-specs?
compile None = Nothing
compile (Val v) = Just $ compileValue v
compile (Mod m) = Just $ compileModifier m
compile (Tex t) = Just $ compileTexture t
compile (Buf b) = Just $ compileBuffer b
compile (Que q) = Just $ compileQueue q