module Hydra.Compile
    ( Compiler
    , compile
    , compileWithRender
    , friendly
    , compact
    )
    where


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
import Hydra.Queue (Queue)
import Hydra.Queue (toUnfoldable) as Queue


type Compiler = { argNames :: Boolean, newLines :: Boolean }


friendly :: Compiler
friendly = { argNames : true, newLines : true }


compact :: Compiler
compact = { argNames : false, newLines : false }


compileOp :: Op -> String
compileOp Plus = "+"
compileOp Subtract = "-"
compileOp Divide = "/"
compileOp Multiply = "*"


compileValue :: Compiler -> Value -> String
compileValue _ (Num n)  = show n
compileValue _ MouseX   = "() => mouse.x"
compileValue _ MouseY   = "() => mouse.y"
compileValue _ Time     = "() => time"
compileValue _ Width    = "x"
compileValue _ Height   = "y"
compileValue _ (Seq xs) = "[" <> (String.joinWith "," $ show <$> xs) <> "]"
compileValue _ Pi       = "Math.PI"
compileValue _ (Harmonic n) = "a.fft[" <> show n <> "]"
compileValue c (Expr v1 op v2) =
    "(" <> compileValue c v1 <> compileOp op <> compileValue c v2 <> ")"
compileValue c (OfTime v) =
    "(time) => " <> compileValue c v


compileModifier :: Compiler -> Modifier -> String
compileModifier compiler mod = compileFn' compiler mod -- compileModifier = compileFn' ???


compileModifiers :: Compiler -> Array Modifier -> String
compileModifiers compiler modifiers =
    String.joinWith
        (if compiler.newLines then "\n    ." else ".")
        $ compileModifier compiler <$> modifiers


compileFn :: forall a. ToFn a Value => Compiler -> a -> String
compileFn compiler =
    compileFnBy compiler (compileValue compiler) <<< toFn


compileFn' :: forall a. ToFn a TextureOrValue => Compiler -> a  -> String
compileFn' compiler =
    compileFnBy
        compiler
        (textureOrValue (compileTexture compiler) (compileValue compiler))
        <<< toFn


compileFnBy :: forall x. Compiler -> (x -> String) -> Fn x -> String
compileFnBy compiler toString (Fn { name, args }) =
    name <> "(" <> (String.joinWith "," $ compileArg <$> Fn.argsToArray args) <> ")"
    where
        compileArg (argName /\ value) =
            (if compiler.argNames then "/*" <> argName <> "*/ " else "") <> toString value


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


compileTexture :: Compiler -> Texture -> String
compileTexture compiler (Texture (Source buf) modifiers) =
    "src(" <> compileBuffer buf <> ")" <>
        (if compiler.newLines then "\n   " else "")
        <> compileModifiers compiler modifiers
compileTexture compiler (Texture source modifiers) =
    compileFn compiler source <>
        (if compiler.newLines then "\n   " else "")
        <> compileModifiers compiler modifiers


compile :: Compiler -> Queue -> String
compile compiler queue =
    String.joinWith "\n\n" $ ouputCode <$> Queue.toUnfoldable queue
    where
        ouputCode (Default /\ tex) =
            compileTexture compiler tex <>
                (if compiler.newLines then "\n   " else "")
                <> ".out()"
        ouputCode (buf     /\ tex) =
            compileTexture compiler tex <>
                (if compiler.newLines then "\n   " else "")
                <> ".out(" <> compileBuffer buf <> ")"


compileWithRender :: Compiler -> Queue -> String
compileWithRender compiler queue  =
    compile compiler queue <> "\n\n    render()"


{- compile :: Hydra -> Maybe String -- TODO: Compile only out-specs?
compile None = Nothing
compile (Val v) = Just $ compileValue v
compile (Mod m) = Just $ compileModifier m
compile (Tex t) = Just $ compileTexture t
compile (Buf b) = Just $ compileBuffer b
compile (Que q) = Just $ compileQueue q -}