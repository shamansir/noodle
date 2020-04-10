module Rpd.Test.Util.Spy
    ( Spy, create, with, with', reset, get, consider
    , wasCalled, callCount, trace, first, last
    , ifError, ifSuccess
    , ifErrorC, ifNoErrorC
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

import Data.Covered


-- `a` is the type of values being traced
-- `x` is the type of value you get as a result of the spy
--     and the type of value being stored and updated after considering each `a`
-- TODO: maybe swap?
data Spy x a = Spy x (Ref x) (x -> a -> x)


-- TODO: rename to `make`
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


first :: forall a. Effect (Spy (Maybe a) a)
first = create Nothing f
  where
    f Nothing x = Just x
    f prev _ = prev


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


ifErrorC :: forall x a. Effect (Spy Boolean (Covered x a))
ifErrorC = create false handler
  where
    handler prev (Carried _) = prev
    handler _ (Recovered _ _) = true


ifNoErrorC :: forall x a. Effect (Spy Boolean (Covered x a))
ifNoErrorC = create false handler
  where
    handler _ (Carried _) = true
    handler prev _ = prev
