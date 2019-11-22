module Rpd.Test.Util.Spy
    ( Spy, create, with, with', reset, get, consider
    , wasCalled, callCount, ifError, ifSuccess
    ) where

import Prelude

import Data.Either (Either(..))

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect (Effect)


data Spy x a = Spy x (Ref x) (x -> a -> x)


create :: forall a x. x -> (x -> a -> x) -> Effect (Spy x a)
create default f =
  Ref.new default >>= \ref -> pure $ Spy default ref f


with :: forall a x z. (a -> Effect z) -> Spy x a -> (a -> Effect z)
with focus spy =
  \a -> do
    consider spy a
    focus a


with' :: forall a x. Spy x a -> (a -> Effect Unit)
with' = consider


consider :: forall a x. Spy x a -> a -> Effect Unit
consider (Spy _ ref f) a =
  Ref.read ref >>= \cur -> Ref.write (f cur a) ref


reset :: forall x a. Spy x a -> Effect Unit
reset (Spy default ref _) =
  Ref.write default ref


-- resetAndGet :: forall x a. Spy x a -> Effect (Spy x a)
-- resetAndGet spy =
--   reset spy >>= (const $ pure spy)


get :: forall x a. Spy x a -> Effect x
get (Spy _ ref _) = Ref.read ref


wasCalled :: forall a. Effect (Spy Boolean a)
wasCalled = create false $ const $ const $ true


callCount :: forall a. Effect (Spy Int a)
callCount = create 0 \prev _ -> prev + 1


ifError :: forall x a. Effect (Spy Boolean (Either x a))
ifError = create false handler
  where
    handler _ (Left _) = true
    handler prev (Right _) = prev


ifSuccess :: forall x a. Effect (Spy Boolean (Either x a))
ifSuccess = create false handler
  where
    handler prev (Left _) = prev
    handler _ (Right _) = true

