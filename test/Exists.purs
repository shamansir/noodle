module Test.Exists where

import Prelude

import Data.Tuple (Tuple)
import Data.Symbol (class IsSymbol)


type Exists f = ∀ r. (∀ a. f a -> r) -> r

-- newtype Exists f = Exists (∀ r. (∀ a. f a -> r) -> r)


newtype Showable = Showable (forall r. (forall a. Show a => a -> r) -> r)

mkShowable :: ∀ a. Show a => a -> Showable
mkShowable a = Showable (_ $ a)


instance showShowable :: Show Showable where
  show (Showable a) = a show


showables :: Array Showable
showables = [mkShowable 1, mkShowable unit, mkShowable "hello"]




newtype HoldsSymbol = HoldsSymbol (forall r. (forall proxy sym. IsSymbol sym => proxy sym -> r) -> r)

holdSymbol :: ∀ proxy sym. IsSymbol sym => proxy sym -> HoldsSymbol
holdSymbol sym = HoldsSymbol (_ $ sym)




data StreamF a s = StreamF s (s -> Tuple s a)


type Stream a = Exists (StreamF a)