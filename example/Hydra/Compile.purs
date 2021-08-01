module Hydra.Compile where


import Prelude ((<>), ($), (<$>), (<#>), (<*>), show, flip)
import Data.String as String
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)

import Hydra (Hydra(..), Value(..))

compileValue :: Value -> String
compileValue (Num n) = show n
compileValue Mouse = "mouse"
compileValue Time = "mouse"
compileValue (Seq xs) = "[" <> (String.joinWith "," $ show <$> xs) <> "]"


compile :: Hydra -> Maybe String -- TODO: Compile only out-specs?
compile None = Nothing
compile (Value v) = Just $ compileValue v
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