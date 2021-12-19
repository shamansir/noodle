module Noodle.Fn
    ( Fn, Named, named
    , BiFn, BiNamed
    , InputId, OutputId
    , make, make'
    , Receive, Pass
    , receive, send
    , ProcessM
    , runProcessM
    , toBiFn, toNamedBiFn
    )
    where


import Prelude

import Data.Maybe (Maybe)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Vec (Vec)
import Data.Vec as Vec
import Data.Typelevel.Num.Sets (class Nat)
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\), (/\))


import Effect.Aff (Aff)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Free (Free, liftF, runFreeM, foldFree)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Error.Class (class MonadThrow, throwError)



data BiFn :: forall (k1 :: Type) (k2 :: Type). k1 -> k2 -> Type -> Type -> Type

data BiFn num_inputs num_outputs i o = BiFn (Vec num_inputs i) (Vec num_outputs o)


data BiNamed :: forall (k1 :: Type) (k2 :: Type). k1 -> k2 -> Type -> Type -> Type

data BiNamed num_inputs num_outputs i o = BiNamed String (BiFn num_inputs num_outputs i o)


data Fn :: forall (k1 :: Type) (k2 :: Type). k1 -> k2 -> Type -> Type

data Fn num_inputs num_outputs a = Fn (BiFn num_inputs num_outputs a a)


data Named :: forall (k1 :: Type) (k2 :: Type). k1 -> k2 -> Type -> Type

data Named num_inputs num_outputs a = Named String (Fn num_inputs num_outputs a)


instance functorBiFn :: Functor (BiFn num_inputs num_outputs b) where
    map f (BiFn inputs outputs) = BiFn inputs (f <$> outputs)


instance bifunctorBiFn :: Bifunctor (BiFn num_inputs num_outputs) where
    bimap fa fb (BiFn inputs outputs) = BiFn (fa <$> inputs) (fb <$> outputs)


instance functorFn :: Functor (Fn num_inputs num_outputs) where
    map f (Fn (BiFn inputs outputs)) = Fn $ BiFn (f <$> inputs) (f <$> outputs)


instance applyFn :: Apply (Fn ni no) where
    apply :: forall a b. Fn ni no (a -> b) -> Fn ni no a -> Fn ni no b
    apply (Fn (BiFn inputsF ouputsF)) (Fn (BiFn inputs outputs))
        = Fn $ BiFn (inputsF <*> inputs) (ouputsF <*> outputs)


instance applicativeFn :: (Nat ni, Nat no) => Applicative (Fn ni no) where
    pure :: forall a. a -> Fn ni no a
    pure a = Fn $ BiFn (pure a) (pure a)


named :: forall ni no a. String -> Fn ni no a -> Named ni no a
named = Named


toBiFn :: forall ni no a. Fn ni no a -> BiFn ni no a a
toBiFn (Fn biFn) = biFn


toNamedBiFn :: forall ni no a. Named ni no a -> BiNamed ni no a a
toNamedBiFn (Named name (Fn biFn)) = BiNamed name biFn


make :: forall ni no a. Nat ni => Nat no => { inputs :: Array a, outputs :: Array a } -> Maybe (Fn ni no a)
make { inputs, outputs } =
    Fn <$> (BiFn <$> Vec.fromArray inputs <*> Vec.fromArray outputs)
    -- Fn
    --     (Vec.fromArray inputs # Maybe.fromMaybe Vec.empty)
    --     (Vec.fromArray outputs # Maybe.fromMaybe Vec.empty)


make' :: forall ni no a. { inputs :: Vec ni a, outputs :: Vec no a } -> Fn ni no a
make' { inputs, outputs } =
    Fn $ BiFn
        inputs
        outputs


newtype InputId = InputId String


newtype OutputId = OutputId String


type Process m d = Receive d -> m (Pass d)


newtype Receive d = Receive { last :: InputId, fromInputs :: InputId /-> d }


instance functorReceive :: Functor Receive where
    map f (Receive { last, fromInputs }) = Receive { last, fromInputs : f <$> fromInputs }


newtype Pass d = Pass { toOutlets :: OutputId /-> d }


instance functorPass :: Functor Pass where
    map f (Pass { toOutlets }) = Pass { toOutlets : f <$> toOutlets }


data ProcessF state d m a
    = State (state -> a /\ state)
    | Lift (m a)
    | Send' OutputId d a
    | Receive' InputId (d -> a)


instance functorNodeF :: Functor m => Functor (ProcessF state d m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        Receive' iid k -> Receive' iid $ map f k
        Send' oid d next -> Send' oid d $ f next


newtype ProcessM state d m a = ProcessM (Free (ProcessF state d m) a)


derive newtype instance functorProcessM :: Functor (ProcessM state d m)
derive newtype instance applyProcessM :: Apply (ProcessM state d m)
derive newtype instance applicativeProcessM :: Applicative (ProcessM state d m)
derive newtype instance bindProcessM :: Bind (ProcessM state d m)
derive newtype instance monadProcessM :: Monad (ProcessM state d m)
derive newtype instance semigroupProcessM :: Semigroup a => Semigroup (ProcessM state d m a)
derive newtype instance monoidProcessM :: Monoid a => Monoid (ProcessM state d m a)
--derive newtype instance bifunctorProcessM :: Bifunctor (ProcessM state d m a)


instance monadEffectNoodleM :: MonadEffect m => MonadEffect (ProcessM state d m) where
  liftEffect = ProcessM <<< liftF <<< Lift <<< liftEffect


instance monadAffNoodleM :: MonadAff m => MonadAff (ProcessM state d m) where
  liftAff = ProcessM <<< liftF <<< Lift <<< liftAff


instance monadStateNoodleM :: MonadState state (ProcessM state d m) where
  state = ProcessM <<< liftF <<< State


instance monadThrowNoodleM :: MonadThrow e m => MonadThrow e (ProcessM state d m) where
  throwError = ProcessM <<< liftF <<< Lift <<< throwError


receive :: forall state d m. InputId -> ProcessM state d m d
receive iid = ProcessM $ liftF $ Receive' iid identity


send :: forall state d m. OutputId -> d -> ProcessM state d m Unit
send oid d = ProcessM $ liftF $ Send' oid d unit


{- program :: forall state d m. MonadEffect m => ProcessM state d m Unit
program = do
    x <- receive "ee"
    n <- liftEffect $ pure 0
    -- modify_ ((+) 1)
    --pure (x + n)
    pure unit -}


imapProcessFFocus :: forall state d d' m a. (d -> d') -> (d' -> d) -> ProcessF state d m ~> ProcessF state d' m
imapProcessFFocus f g =
    case _ of
        State k -> State k
        Lift m -> Lift m
        Receive' iid k -> Receive' iid (k <<< g)
        Send' oid d next -> Send' oid (f d) next


imapProcessMFocus :: forall state d d' m a. (d -> d') -> (d' -> d) -> ProcessM state d m ~> ProcessM state d' m
imapProcessMFocus f g (ProcessM processFree) =
    ProcessM $ foldFree (liftF <<< imapProcessFFocus f g) processFree
    --ProcessM $ liftF $ imapProcessFFocus f g processFree


runProcessM :: forall state d. d -> state -> ProcessM state d Aff ~> Aff
runProcessM default state (ProcessM processFree) = do
    stateRef <- liftEffect $ Ref.new state
    runProcessFreeM default stateRef processFree


runProcessFreeM :: forall state d. d -> Ref state -> Free (ProcessF state d Aff) ~> Aff
runProcessFreeM default stateRef =
    --foldFree go-- (go stateRef)
    runFreeM go
    where
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState nextState
                    pure next
        go (Lift m) = m
        go (Receive' _ getV) = pure $ getV default
        go (Send' _ _ next) = pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState nextState = liftEffect $ Ref.write nextState stateRef



-- run :: forall state d m a. state -> Network d -> NoodleM state d m a -> Effect (state /\ Network d)
-- run state nw = case _ of
--     _ -> pure $ state /\ nw
