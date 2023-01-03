module Test.Symbols where

import Prelude



import Data.Symbol
import Unsafe.Coerce (unsafeCoerce)

import Type.Proxy (Proxy)


data MySymbolType (s :: Symbol) = MySymbolType


newtype AnotherSymbol (s :: Symbol) = AnotherSymbol String




testReflect :: forall s. AnotherSymbol s -> String
testReflect (AnotherSymbol s) = s


materialize :: forall s. IsSymbol s => MySymbolType s -> AnotherSymbol s
materialize = reflectSymbol >>> AnotherSymbol