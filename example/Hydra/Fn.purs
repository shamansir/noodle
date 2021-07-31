module Hydra.Fn where


import Hydra (Hydra(..))


type Fn = { name :: String, args :: Array String, out :: String }


class ToFn a where
    toFn :: a -> Fn
