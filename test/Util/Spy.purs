module Rpd.Test.Util.Spy
    ( Spy, create, on, reset
    ) where

import Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect (Effect)


data Spy x = Spy x (Ref x) (forall a. a -> x)


create :: forall x. x -> (forall a. a -> x) -> Effect (Spy x)
create default f =
  Ref.new default >>= \ref -> pure $ Spy default ref f


on :: forall a x z. (a -> Effect z) -> Spy x -> (a -> Effect z)
on focus (Spy _ ref f) =
  \a -> Ref.write (f a) ref >>= (const $ focus a)


reset :: forall x. Spy x -> Effect (Spy x)
reset spy@(Spy default ref _) =
  Ref.write default ref >>= (const $ pure spy)



