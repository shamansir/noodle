module Blessed.Internal.BlessedOp where


import Prelude


import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.State as State

import Data.Bifunctor (lmap, rmap, bimap)
import Data.Either (Either)
import Data.Either as Either
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), decode) as CA
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (stringify) as Json
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error as ADE

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)

import Blessed.Internal.Foreign (encodeCommand, commandToJson) as Foreign
import Blessed.Internal.Command (Command) as I
import Blessed.Internal.NodeKey as I
import Blessed.Internal.JsApi as I
import Blessed.Internal.Dump as Dump
import Blessed.Internal.BlessedSubj as K



data BlessedOpF state m a
    = GetStateRef (Ref state -> a)
    | State (state -> a /\ state)
    | Lift (m a)
    | PerformOne I.RawNodeKey I.Command a
    | PerformSome I.RawNodeKey (Array I.Command) a
    | PerformGet I.RawNodeKey I.Command (Json -> a)
    | PerformOnProcess I.Command a


instance functorBlessedOpF :: Functor m => Functor (BlessedOpF state m) where
    map f = case _ of
        GetStateRef k -> GetStateRef $ map f k
        State k -> State $ lmap f <<< k
        Lift m -> Lift $ map f m
        PerformOne nid cmd a -> PerformOne nid cmd $ f a
        PerformSome nid cmds a -> PerformSome nid cmds $ f a
        PerformGet nid getCmd k -> PerformGet nid getCmd $ map f k
        PerformOnProcess cmd a -> PerformOnProcess cmd $ f a


type BlessedOp state m = BlessedOpM state m Unit
type BlessedOpDef state m = Ref state -> BlessedOpM state m Unit
type BlessedOpJsonGet state m a = BlessedOpM state m (Either CA.JsonDecodeError a)
type BlessedOpGet state m a = BlessedOpM state m a



newtype BlessedOpM state m a = BlessedOpM (Free (BlessedOpF state m) a)


derive newtype instance functorBlessedOpM :: Functor (BlessedOpM state m)
derive newtype instance applyBlessedOpM :: Apply (BlessedOpM state m)
derive newtype instance applicativeBlessedOpM :: Applicative (BlessedOpM state m)
derive newtype instance bindBlessedOpM :: Bind (BlessedOpM state m)
derive newtype instance monadBlessedOpM :: Monad (BlessedOpM state m)
derive newtype instance semigroupBlessedOpM :: Semigroup a => Semigroup (BlessedOpM state m a)
derive newtype instance monoidBlessedOpM :: Monoid a => Monoid (BlessedOpM state m a)


instance monadEffectBlessedOpM :: MonadEffect m => MonadEffect (BlessedOpM state m) where
  liftEffect = BlessedOpM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAfBlessedOpM :: MonadAff m => MonadAff (BlessedOpM state m) where
  liftAff = BlessedOpM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateBlessedOpM :: MonadState state (BlessedOpM state m) where
  state = BlessedOpM <<< Free.liftF <<< State


instance monadThrowBlessedOpM :: MonadThrow e m => MonadThrow e (BlessedOpM state m) where
  throwError = BlessedOpM <<< Free.liftF <<< Lift <<< throwError


instance monadRecBlessedOpM :: MonadRec (BlessedOpM state m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


getStateRef :: forall state m. BlessedOpGet state m (Ref state)
getStateRef = BlessedOpM $ Free.liftF $ GetStateRef identity


{- Processing -}


perform :: forall state m. I.RawNodeKey -> I.Command -> BlessedOp state m
perform nid cmd = BlessedOpM $ Free.liftF $ PerformOne nid cmd unit


performGet :: forall state m a. CA.JsonCodec a -> I.RawNodeKey -> I.Command -> BlessedOpJsonGet state m a
performGet codec nid cmd = BlessedOpM $ Free.liftF $ PerformGet nid cmd $ CA.decode codec


performGet' :: forall state m a. DecodeJson a => I.RawNodeKey -> I.Command -> BlessedOpJsonGet state m a
performGet' nid cmd = BlessedOpM $ Free.liftF $ PerformGet nid cmd $ (decodeJson >>> lmap convertJsonError)


performSome :: forall state m. I.RawNodeKey -> Array I.Command -> BlessedOp state m
performSome nid cmds = BlessedOpM $ Free.liftF $ PerformSome nid cmds unit


performOnProcess :: forall state m. I.Command -> BlessedOp state m
performOnProcess cmd = BlessedOpM $ Free.liftF $ PerformOnProcess cmd unit


-- type Performer m = I.Command -> m Unit -- TODO: return m (Maybe Json), for getters


lift :: forall state m. m Unit -> BlessedOpM state m Unit
lift m = BlessedOpM $ Free.liftF $ Lift m


runM
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => state
    -> BlessedOpM state m
    ~> m
runM state blessedFree =
    liftEffect (Ref.new state) >>= \stateRef -> runM' stateRef blessedFree -- flip?


runM'
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => Ref state
    -> BlessedOpM state m
    ~> m
runM' stateRef (BlessedOpM blessedFree) =
    runFreeM stateRef blessedFree


runFreeM
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => Ref state
    -> Free (BlessedOpF state m)
    ~> m
runFreeM stateRef fn = do
    Free.runFreeM go fn
    where
        go :: forall a. BlessedOpF state m a -> m a

        go (GetStateRef getV) =
            pure $ getV stateRef

        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState state nextState
                    pure next

        go (Lift m) = m

        go (PerformOne target cmd next) = do
            _ <- liftEffect $ callForeignCommand target cmd
            Dump.command cmd
            pure next

        go (PerformSome target cmds next) = do
            _ <- traverse (liftEffect <<< callForeignCommand target) cmds
            traverse_ Dump.command cmds
            pure next

        go (PerformGet target cmd getV) = do
            value <- liftEffect $ callForeignCommand target cmd
            Dump.command cmd
            pure $ getV value

        go (PerformOnProcess cmd next) = do
            _ <- liftEffect $ callForeignCommand (I.rawify I.process) cmd
            Dump.command cmd
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState _ nextState = liftEffect $ Ref.modify_ (const nextState) stateRef
        callForeignCommand target cmd =
            case Foreign.encodeCommand cmd of
                cmd_ /\ [] -> callCommand_ target cmd_
                cmd_ /\ handlers -> callCommandEx_ target cmd_ handlers


makeHandler :: forall state subj sym. I.NodeKey subj sym -> I.EventId -> Array Json -> (I.NodeKey subj sym -> I.EventJson -> BlessedOp state Effect) -> I.SHandler state
makeHandler nodeKey eventId arguments op =
    I.SHandler eventId arguments
        $ \stateRef rawNodeKey evtJson -> do
            -- TODO: check IDs match?
            Dump.handlerCall rawNodeKey eventId arguments
            runM' stateRef $ op nodeKey evtJson


convertJsonError :: ADE.JsonDecodeError -> CA.JsonDecodeError
convertJsonError = case _ of
    ADE.TypeMismatch s -> CA.TypeMismatch s
    ADE.UnexpectedValue json -> CA.UnexpectedValue json
    ADE.AtIndex n jde -> CA.AtIndex n $ convertJsonError jde
    ADE.AtKey n jde -> CA.AtKey n $ convertJsonError jde
    ADE.Named n jde -> CA.Named n $ convertJsonError jde
    ADE.MissingValue -> CA.MissingValue


foreign import execute_ :: I.BlessedEnc -> Effect Unit
foreign import registerNode_ :: I.NodeEnc -> Effect Unit
foreign import callCommand_ :: I.RawNodeKey -> I.CommandEnc -> Effect Json
foreign import callCommandEx_ :: I.RawNodeKey -> I.CommandEnc -> Array I.HandlerCallEnc -> Effect Json
