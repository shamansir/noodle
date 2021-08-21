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
compileValue X = "x"
compileValue Y = "y"
compileValue (Seq xs) = "[" <> (String.joinWith "," $ show <$> xs) <> "]"


compileModifier :: Modifier -> String
compileModifier mod = compileFn' mod -- compileModifier = compileFn' ???


compileFn :: forall a. ToFn a Value => a -> String
compileFn = compileFnBy compileValue <<< toFn


compileFn' :: forall a. ToFn a EntityOrValue => a -> String
compileFn' = compileFnBy (either compileEntity compileValue) <<< toFn


compileFnBy :: forall x. (x -> String) -> Fn x -> String
compileFnBy toString (Fn { name, args })  =
    name <> "(" <> (String.joinWith "," $ compileArg <$> Fn.argsToArray args) <> ")"
    where compileArg (argName /\ value) = "/*" <> argName <> "*/ " <> toString value


compileEntity :: Entity -> String
compileEntity (Entity source modifiers) =
    if Array.length modifiers > 0
        then compileFn source <> "\n    ." <> (String.joinWith "\n    ." $ compileModifier <$> modifiers)
        else compileFn source


compileQueue :: Queue -> String
compileQueue queue =
    String.joinWith "\n\n" $ ouputCode <$> queue
    where
        ouputCode (e /\ Default) = compileEntity (Debug.spy "e" e)  <> "\n   .out()"
        ouputCode (e /\ Output n) = compileEntity e <> "\n   .out(" <> show n <> ")"


compile :: Hydra -> Maybe String -- TODO: Compile only out-specs?
compile None = Nothing
compile (Value v) = Just $ compileValue v
compile (Hydra e) = Just $ compileEntity e
compile (Out q) = Just $ compileQueue q