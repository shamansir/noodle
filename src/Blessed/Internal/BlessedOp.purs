module Blessed.Internal.BlessedOp where


import Prelude

import Prelude

import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.List (List)
import Data.Traversable (traverse)

import Prim.RowList as RL
import Record.Extra (class Keys, keys)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)

import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
-- import Noodle.Fn.Protocol (Protocol)

import Prim.Row (class Cons)
import Record as Record
import Record.Unsafe (unsafeGet, unsafeSet, unsafeDelete) as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import Data.Argonaut.Core (Json)

import Blessed.Internal.Command as I
import Blessed.Internal.JsApi as I
import Blessed.Internal.Codec (encodeCommand)



data BlessedOpF state m a
    = State (state -> a /\ state)
    | Lift (m a)
    | PerformOne I.NodeId I.Command a
    | PerformSome I.NodeId (Array I.Command) a
    | PerformOnProcess I.Command a


instance functorBlessedOpF :: Functor m => Functor (BlessedOpF state m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        PerformOne nid cmd a -> PerformOne nid cmd $ f a
        PerformSome nid cmds a -> PerformSome nid cmds $ f a
        PerformOnProcess cmd a -> PerformOnProcess cmd $ f a


type BlessedOp m = BlessedOpM I.Registry m Unit



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


{- Processing -}

perform :: forall m. I.NodeId -> I.Command -> BlessedOpM I.Registry m Unit
perform nid cmd = BlessedOpM $ Free.liftF $ PerformOne nid cmd unit


performSome :: forall m. I.NodeId -> Array I.Command -> BlessedOpM I.Registry m Unit
performSome nid cmds = BlessedOpM $ Free.liftF $ PerformSome nid cmds unit


performOnProcess :: forall m. I.Command -> BlessedOpM I.Registry m Unit
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
runM state (BlessedOpM blessedFree) =
    liftEffect (Ref.new state) >>= \stateRef -> runFreeM stateRef blessedFree


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
        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState state nextState
                    pure next
        go (Lift m) = m
        go (PerformOne target cmd next) = do
            _ <- liftEffect $ callCommand_ target $ encodeCommand cmd
            pure next

        go (PerformSome target cmds next) = do
            _ <- traverse (liftEffect <<< callCommand_ target <<< encodeCommand) cmds
            pure next

        go (PerformOnProcess cmd next) = do
            _ <- liftEffect $ callCommand_ (I.NodeId "process") $ encodeCommand cmd
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState _ nextState = liftEffect $ Ref.modify_ (const nextState) stateRef


makeHandler :: I.EventId -> Array Json -> (I.NodeId -> Json -> BlessedOp Effect) -> I.SHandler
makeHandler eventId arguments op =
    I.SHandler eventId arguments
        $ \registry nodeId (I.EventJson evt) ->
            runM (I.unveilRegistry registry) $ op nodeId $ evt



foreign import execute_ :: I.BlessedEnc -> Effect Unit
foreign import registerNode_ :: I.NodeEnc -> Effect Unit
foreign import callCommand_ :: I.NodeId -> I.CommandEnc -> Effect Json