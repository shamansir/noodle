module Hydra.Compile where


import Prelude ((<>), ($), (<$>), (<#>), (<*>), show, flip)
import Data.String as String
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)

import Hydra (Hydra(..), Value(..))


compile :: Hydra -> Maybe String
compile None = Nothing
compile (Osc v1 v2 v3) =
    (\v1 v2 v3 -> "osc(" <> v1 <> "," <> v2 <> "," <> v3 <> ")")
        <$> (compile $ Value' v1)
        <*> (compile $ Value' v2)
        <*> (compile $ Value' v3)
compile (Value' (Num n)) = Just $ show n
compile (Value' Mouse) = Just "mouse"
compile (Value' Time) = Just "time"
compile (Value' (Seq xs)) =
    Just $ "[" <> (String.joinWith "," $ show <$> xs) <> "]"
compile (Join hydras) =
    String.joinWith "\n\n" <$> (sequence $ compile <$> hydras)
compile (Out maybeN hydra) =
    compile hydra <#> (flip (<>) $ ".out(" <> (maybe "" show maybeN) <> ")")