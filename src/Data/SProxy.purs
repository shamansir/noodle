module Data.SProxy where

import Prelude
import Data.Symbol (class IsSymbol)
import Data.Symbol (reflectSymbol) as S

import Type.Proxy (Proxy)


-- Temprorary replacement for deprecated `SProxy`

class SProxy proxy where
    proxify :: forall sym. IsSymbol sym => proxy sym -> Proxy sym


-- fromProxy :: forall proxy sym. SProxy proxy => Proxy sym -> sproxy sym
-- fromProxy


instance SProxy Proxy where
    proxify = identity


reflectSymbol :: forall proxy sym. SProxy proxy => IsSymbol sym => proxy sym -> String
reflectSymbol = S.reflectSymbol <<< proxify