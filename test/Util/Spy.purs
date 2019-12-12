module Rpd.Test.Util.Spy
    ( Spy, create, with, reset, get, consider
    , wasCalled, callCount, trace, last
    , ifError, ifSuccess
    , ifErrors, ifNoErrors
    , contramap
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (snoc)

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect (Effect)


data Spy x a = Spy x (Ref x) (x -> a -> x)


create :: forall a x. x -> (x -> a -> x) -> Effect (Spy x a)
create default f =
  Ref.new default >>= \ref -> pure $ Spy default ref f


with :: forall a x. Spy x a -> (a -> Effect Unit)
with = consider


with' :: forall a x z. (a -> Effect z) -> Spy x a -> (a -> Effect z)
with' focus spy =
  \a -> do
    consider spy a
    focus a


consider :: forall a x. Spy x a -> a -> Effect Unit
consider (Spy _ ref f) a =
  Ref.read ref >>= \cur -> Ref.write (f cur a) ref


consider' :: forall a x. Spy x a -> a -> Effect a
consider' (Spy _ ref f) a =
  Ref.read ref >>=
    \cur -> Ref.write (f cur a) ref >>=
      (const $ pure a)


reset :: forall x a. Spy x a -> Effect Unit
reset (Spy default ref _) =
  Ref.write default ref


-- resetAndGet :: forall x a. Spy x a -> Effect (Spy x a)
-- resetAndGet spy =
--   reset spy >>= (const $ pure spy)


-- TODO: Contravariant instance
contramap :: forall a b x. (b -> a) -> Spy x a -> Spy x b
contramap f (Spy default ref spyF) =
  Spy default ref \x b -> spyF x $ f b


get :: forall x a. Spy x a -> Effect x
get (Spy _ ref _) = Ref.read ref


wasCalled :: forall a. Effect (Spy Boolean a)
wasCalled = create false $ const $ const $ true


callCount :: forall a. Effect (Spy Int a)
callCount = create 0 \prev _ -> prev + 1


last :: forall a. Effect (Spy (Maybe a) a)
last = create Nothing $ const Just


-- trace :: forall a. Effect (Spy (Seq a) a)
-- trace = create Seq.empty (+>)


trace :: forall a. Effect (Spy (Array a) a)
trace = create [] snoc


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


ifErrors :: forall x a. Effect (Spy Boolean (Array x /\ a))
ifErrors = create false handler
  where
    handler prev ([] /\ _) = prev
    handler _ _ = true


ifNoErrors :: forall x a. Effect (Spy Boolean (Array x /\ a))
ifNoErrors = create false handler
  where
    handler _ ([] /\ _) = true
    handler prev _ = prev
