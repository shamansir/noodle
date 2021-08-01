module Hydra.Compile where


import Prelude ((<>), ($), (<$>), (<#>), (<*>), show, flip, (<<<))
import Data.String as String
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Array (zip)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))

import Hydra
import Hydra.Fn (class ToFn, Fn(..), toFn)


compileValue :: Value -> String
compileValue (Num n) = show n
compileValue Mouse = "mouse"
compileValue Time = "mouse"
compileValue (Seq xs) = "[" <> (String.joinWith "," $ show <$> xs) <> "]"


compileFn :: forall a. ToFn a Value => a -> String
compileFn = compileFn' <<< toFn


compileFn' :: Fn Value -> String
compileFn' (Fn { name, args })  =
    name <> "(" <> (String.joinWith "," $ compileArg <$> args) <> ")"
    where compileArg (argName /\ value) = "/* " <> argName <> "*/ " <> compileValue value


compileEntity :: Entity -> String
compileEntity (Entity source modifiers) =
    compileFn source <> "." <>
        (String.joinWith "\n    " $ compileModifier <$> modifiers)


compileModifier :: Modifier -> String
compileModifier _ = "" -- TODO


{- compileEFn :: forall a. ToFn a EntityOrValue => a -> Entity -> Array Value -> String
compileEFn = compileFn' <<< toFn -}


compile :: Hydra -> Maybe String -- TODO: Compile only out-specs?
compile None = Nothing
compile (Value v) = Just $ compileValue v
compile (Hydra e) = Just $ compileEntity e
{- compile (Osc v1 v2 v3) =
    (\v1 v2 v3 -> "osc(" <> v1 <> "," <> v2 <> "," <> v3 <> ")")
        <$> (compile $ Value' v1)
        <*> (compile $ Value' v2)
        <*> (compile $ Value' v3) -}
compile _ = Nothing

{- compile (Join hydras) =
    String.joinWith "\n\n" <$> (sequence $ compile <$> hydras)
compile (Out maybeN hydra) =
    compile hydra <#> (flip (<>) $ ".out(" <> (maybe "" show maybeN) <> ")") -}
