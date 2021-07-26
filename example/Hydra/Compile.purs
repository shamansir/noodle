module Hydra.Compile where


import Prelude ((<>), ($), (<$>), show)
import Data.String as String
import Data.Maybe (maybe)

import Hydra (Hydra(..), Value(..))


compile :: Hydra -> String
compile None = "undefined"
compile (Osc v1 v2 v3) =
    "osc("
        <> (compile $ Value' v1)
        <> ","
        <> (compile $ Value' v2)
        <> ","
        <> (compile $ Value' v3)
        <> ")"
compile (Value' (Num n)) = show n
compile (Value' Mouse) = "mouse"
compile (Value' Time) = "time"
compile (Value' (Seq xs)) = "[" <> (String.joinWith "," $ show <$> xs) <> "]"
compile (Join hydras) = String.joinWith "\n\n" $ compile <$> hydras
compile (Out hydra maybeN) = compile hydra <> ".out(" <> (maybe "" show maybeN) <> ")"