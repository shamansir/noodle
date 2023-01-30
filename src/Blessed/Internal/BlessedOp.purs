module Blessed.Internal.BlessedOp where


import Prelude


import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Data.Bifunctor (lmap, rmap)
import Data.Either (Either)
import Data.Either as Either
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\), type (/\))

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

import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile)
import Node.Path (FilePath)

import Blessed.Internal.Codec (encodeCommand, commandToJson, encodeDump)
import Blessed.Internal.Command (Command) as I
import Blessed.Internal.NodeKey as I
import Blessed.Internal.JsApi as I
import Blessed.Internal.BlessedSubj as K



commandsDumpPath :: FilePath
commandsDumpPath = "./commands_dump.txt"


data BlessedOpF state m a
    = State (state -> a /\ state)
    | Lift (m a)
    | PerformOne I.RawNodeKey I.Command a
    | PerformSome I.RawNodeKey (Array I.Command) a
    | PerformGet I.RawNodeKey I.Command (Json -> a)
    | PerformOnProcess I.Command a


instance functorBlessedOpF :: Functor m => Functor (BlessedOpF state m) where
    map f = case _ of
        State k -> State (lmap f <<< k)
        Lift m -> Lift (map f m)
        PerformOne nid cmd a -> PerformOne nid cmd $ f a
        PerformSome nid cmds a -> PerformSome nid cmds $ f a
        PerformGet nid getCmd k -> PerformGet nid getCmd $ map f k
        PerformOnProcess cmd a -> PerformOnProcess cmd $ f a


type BlessedOp m = BlessedOpM I.Registry m Unit
type BlessedOpG m a = BlessedOpM I.Registry m (Either CA.JsonDecodeError a)



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

perform :: forall m. I.RawNodeKey -> I.Command -> BlessedOp m
perform nid cmd = BlessedOpM $ Free.liftF $ PerformOne nid cmd unit


performGet :: forall m a. CA.JsonCodec a -> I.RawNodeKey -> I.Command -> BlessedOpG m a
performGet codec nid cmd = BlessedOpM $ Free.liftF $ PerformGet nid cmd $ CA.decode codec


performGet' :: forall m a. DecodeJson a => I.RawNodeKey -> I.Command -> BlessedOpG m a
performGet' nid cmd = BlessedOpM $ Free.liftF $ PerformGet nid cmd $ (decodeJson >>> lmap convertJsonError)


performSome :: forall m. I.RawNodeKey -> Array I.Command -> BlessedOp m
performSome nid cmds = BlessedOpM $ Free.liftF $ PerformSome nid cmds unit


performOnProcess :: forall m. I.Command -> BlessedOp m
performOnProcess cmd = BlessedOpM $ Free.liftF $ PerformOnProcess cmd unit


-- type Performer m = I.Command -> m Unit -- TODO: return m (Maybe Json), for getters


lift :: forall state m. m Unit -> BlessedOpM state m Unit
lift m = BlessedOpM $ Free.liftF $ Lift m


dumpCommand :: forall m. MonadEffect m => I.Command -> m Unit
dumpCommand =
    liftEffect
        <<< launchAff_
        <<< appendTextFile UTF8 commandsDumpPath
        <<< (<>) "\n"
        <<< Json.stringify
        <<< commandToJson


dumpHandlerCall :: forall m. MonadEffect m => I.RawNodeKey -> I.EventId -> Array Json -> m Unit
dumpHandlerCall (I.RawNodeKey nodeKey) (I.EventId event) args =
    liftEffect
        $ launchAff_
        $ appendTextFile UTF8 commandsDumpPath
        $ (<>) "\n"
        $ Json.stringify
        $ encodeDump
        $ I.CallDump
            { args, event, nodeId : nodeKey.id
            , nodeSubj : K.toString nodeKey.subject
            }


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
            dumpCommand cmd
            pure next

        go (PerformSome target cmds next) = do
            _ <- traverse (liftEffect <<< callCommand_ target <<< encodeCommand) cmds
            traverse_ dumpCommand cmds
            pure next

        go (PerformGet target cmd getV) = do
            value <- liftEffect $ callCommand_ target $ encodeCommand cmd
            dumpCommand cmd
            pure $ getV $ value

        go (PerformOnProcess cmd next) = do
            _ <- liftEffect $ callCommand_ (I.rawify I.process) $ encodeCommand cmd
            dumpCommand cmd
            pure next

        getUserState = liftEffect $ Ref.read stateRef
        writeUserState _ nextState = liftEffect $ Ref.modify_ (const nextState) stateRef


makeHandler :: forall subj sym. I.NodeKey subj sym -> I.EventId -> Array Json -> (I.NodeKey subj sym -> Json -> BlessedOp Effect) -> I.SHandler
makeHandler nodeKey eventId arguments op =
    I.SHandler eventId arguments
        $ \registry rawNodeKey (I.EventJson evt) -> do
            -- TODO: check IDs match?
            dumpHandlerCall rawNodeKey eventId arguments
            runM (I.unveilRegistry registry) $ op nodeKey $ evt


foreign import execute_ :: I.BlessedEnc -> Effect Unit
foreign import registerNode_ :: I.NodeEnc -> Effect Unit
foreign import callCommand_ :: I.RawNodeKey -> I.CommandEnc -> Effect Json


convertJsonError :: ADE.JsonDecodeError -> CA.JsonDecodeError
convertJsonError = case _ of
    ADE.TypeMismatch s -> CA.TypeMismatch s
    ADE.UnexpectedValue json -> CA.UnexpectedValue json
    ADE.AtIndex n jde -> CA.AtIndex n $ convertJsonError jde
    ADE.AtKey n jde -> CA.AtKey n $ convertJsonError jde
    ADE.Named n jde -> CA.Named n $ convertJsonError jde
    ADE.MissingValue -> CA.MissingValue