module Test.Toolkit2 where

import Prelude


import Effect.Aff (Aff)

import Data.Array as Array
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reifySymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (snd, fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Heterogeneous.Mapping as H

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Noodle.Channel as Ch
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Node (Node)
import Noodle.Node as Node

import Prim.Row (class Cons)
import Record as Record
import Record.Extra as Record

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT

import Test.Signal (expectFn, expect)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.State as State



import Noodle.Toolkit2 as T


intChan = Ch.hot "int" 0
strChan = Ch.hot "str" ""


type Nodes =
    ( sum :: Node.NodeFn Unit Int
    , sum2 :: Node.NodeFn String Int
    , concat :: Node.NodeFn Int String
    )


type States =
    ( sum :: Unit
    , concat :: Int
    )


states :: Record States
states =
    { sum : unit
    , concat : 0
    }


-- _sum = Proxy :: Proxy "sum"
_sum = T.family Proxy :: T.Family "sum"
_sum2 = T.family Proxy :: T.Family "sum2"
_concat = T.family Proxy :: T.Family "concat"
_foo = T.family Proxy :: T.Family "foo"


toolkit :: T.Toolkit Nodes
toolkit =
    T.from
        { sum :
            Fn.make "sum"
                -- TODO: withInlets / withInputs ...
                    -- -< "a" /\ intChan
                [ Fn.in_ "a" /\ intChan
                , Fn.in_ "b" /\ intChan
                ]
                -- TODO: withOutlets / withInputs ...
                    -- >- "a" /\ intChan
                [ Fn.out_ "sum" /\ intChan
                ]
            $ do
                a <- Fn.receive $ Fn.in_ "a"  -- TODO: some operator i.e. <<+ "a"
                b <- Fn.receive $ Fn.in_ "b"  -- TODO: some operator i.e. <<+ "b"
                Fn.send (Fn.out_ "sum") $ a + b  -- TODO: some operator i.e. +>> "b"
        , sum2 :
            Fn.make "sum2"
                -- TODO: withInlets / withInputs ...
                    -- -< "a" /\ intChan
                [ Fn.in_ "a" /\ intChan
                , Fn.in_ "b" /\ intChan
                ]
                -- TODO: withOutlets / withInputs ...
                    -- >- "a" /\ intChan
                [ Fn.out_ "sum" /\ intChan
                ]
            $ do
                a <- Fn.receive $ Fn.in_ "a"
                b <- Fn.receive $ Fn.in_ "b"
                -- State.modify_ (map $ const $ show $ a - b)
                Fn.send (Fn.out_ "sum") $ a + b
        , concat :
            Fn.make "concat"
                -- TODO: withInlets / withInputs ...
                    -- -< "a" /\ intChan
                [ Fn.in_ "a" /\ strChan
                , Fn.in_ "b" /\ strChan
                ]
                -- TODO: withOutlets / withInputs ...
                    -- >- "a" /\ intChan
                [ Fn.out_ "concat" /\ strChan
                ]
            $ do
                a <- Fn.receive $ Fn.in_ "a"  -- TODO: some operator i.e. <<+ "a"
                b <- Fn.receive $ Fn.in_ "b"  -- TODO: some operator i.e. <<+ "b"
                Fn.send (Fn.out_ "concat") $ a <> b  -- TODO: some operator i.e. +>> "b"
        }


spawnSum ∷ ∀ m. Functor m ⇒ MonadEffect m ⇒ Int → m (Node Unit Int)
spawnSum = T.spawn toolkit _sum


spawnSum2 ∷ ∀ m. Functor m ⇒ MonadEffect m ⇒ Int → m (Node String Int)
spawnSum2 = T.spawn toolkit _sum2


spawnConcat ∷ ∀ m. Functor m ⇒ MonadEffect m ⇒ String → m (Node Int String)
spawnConcat = T.spawn toolkit _concat

-- fails:
{-
spawnFoo ∷ ∀ state d m. Functor m ⇒ MonadEffect m ⇒ String → m (Node state d)
spawnFoo = T.spawn toolkit _foo
-}


-- testSequence = (?wh :: forall m. Applicative m => m (forall a b. a -> b)) <*> Record.sequenceRecord (T.toRecord toolkit)
-- testSequence = ?wh $ Record.sequenceRecord (T.toRecord toolkit)



testToStates :: H.HMapWithIndex T.ToState (Record Nodes) (Record States) => Record States
testToStates = T.toStates toolkit


unsafeSpawn ∷
    forall (m ∷ Type -> Type) (s ∷ Symbol) (r ∷ Row Type) state d
     . IsSymbol s
    => Cons s (Node.NodeFn state d) r Nodes
    => MonadEffect m
    => String
    -> d
    -> m (Maybe (T.Family s /\ (Node state d)))
unsafeSpawn = T.unsafeSpawn toolkit


-- spawnSum' ∷ ∀ m s. Functor m ⇒ IsSymbol s => MonadEffect m ⇒ Int → m (Maybe (T.Family s /\ Node Unit Int))
unsafeSpawnSum ∷
    forall (m ∷ Type -> Type) (r ∷ Row Type)
     . Cons "sum" (Node.NodeFn Unit Int) r Nodes
    => MonadEffect m
    => Int
    -> m (Maybe (T.Family "sum" /\ (Node Unit Int)))
unsafeSpawnSum n = unsafeSpawn "sum" n


unsafeSpawnSum' ∷
    forall (m ∷ Type -> Type) (s ∷ Symbol) (r ∷ Row Type)
     . Cons "sum" (Node.NodeFn Unit Int) r Nodes
    => MonadEffect m
    => Int
    -> m (Maybe (Node Unit Int))
unsafeSpawnSum' n = map Tuple.snd <$> unsafeSpawnSum n



unsafeSpawnSum2 ∷
    forall (m ∷ Type -> Type) (r ∷ Row Type)
     . Cons "sum2" (Node.NodeFn String Int) r Nodes
    => MonadEffect m
    => Int
    -> m (Maybe (T.Family "sum2" /\ (Node String Int)))
unsafeSpawnSum2 n = unsafeSpawn "sum2" n


{-
trySpawn' ∷
    forall (m ∷ Type -> Type) (s ∷ Symbol) (r ∷ Row Type) state d
     . IsSymbol s
    => Cons s (Node.NodeFn state d) r Nodes
    => MonadEffect m
    => String
    -> d
    -> m (Maybe ((T.Family s) /\ (Node state d)))
trySpawn' = T.trySpawn' toolkit
-}




spec :: Spec Unit
spec = do

    describe "foo" $ do

        it "spawning works" $ liftEffect $ do
            node <- spawnSum 0
            Node.run' node -- FIXME: may be no need in `imapState` and joining it with patch state
            Node.send node (Fn.in_ "a" /\ 5) -- TODO: some operator i.e. node +> "a" /\ 5
            Node.send node (Fn.in_ "b" /\ 3) -- TODO: some operator i.e. node +> "b" /\ 3
            sum <- Node.getO node (Fn.out_ "sum") -- TODO: some operator i.e. v <- "sum" <+ node
            shouldEqual sum 8

        it "unsafe spawning works" $ do
            maybeNode <- unsafeSpawnSum 0 # liftEffect -- or `spawnAndRun`
            case maybeNode of
                Just ( _ /\ node ) -> liftEffect $ do -- do inside `NodeM` ?
                    Node.run' node -- FIXME: may be no need in `imapState` and joining it with patch state
                    Node.send node (Fn.in_ "a" /\ 5) -- TODO: some operator i.e. node +> "a" /\ 5
                    Node.send node (Fn.in_ "b" /\ 3) -- TODO: some operator i.e. node +> "b" /\ 3
                    sum <- Node.getO node (Fn.out_ "sum") -- TODO: some operator i.e. v <- "sum" <+ node
                    shouldEqual sum 8
                Nothing ->
                    fail "node wasn't spawned"

            pure unit

        it "spawning with state works" $ liftEffect $ do
            node <- spawnSum2 0
            Console.log $ show "before everything"
            stateSig <- Node.run "---" node
            stateAtStart <- Signal.get stateSig
            stateAtStart `shouldEqual` "0"
            Node.send node (Fn.in_ "a" /\ 5) -- TODO: some operator i.e. node +> "a" /\ 5
            stateAfterFirstInlet <- Signal.get stateSig
            stateAfterFirstInlet `shouldEqual` "5"
            Node.send node (Fn.in_ "b" /\ 3) -- TODO: some operator i.e. node +> "b" /\ 3
            finalState <- Signal.get stateSig
            finalState `shouldEqual` "2"
            pure unit

        it "unsafe spawning with state works" $ do
            maybeNode <- unsafeSpawnSum2 0 # liftEffect
            case maybeNode of
                Just ( _ /\ node ) -> liftEffect $ do -- do inside `NodeM` ?
                    Console.log $ show "before everything"
                    stateSig <- Node.run "--" node
                    stateAtStart <- Signal.get stateSig
                    stateAtStart `shouldEqual` "0"
                    Node.send node (Fn.in_ "a" /\ 5) -- TODO: some operator i.e. node +> "a" /\ 5
                    stateAfterFirstInlet <- Signal.get stateSig
                    stateAfterFirstInlet `shouldEqual` "5"
                    Node.send node (Fn.in_ "b" /\ 3) -- TODO: some operator i.e. node +> "b" /\ 3
                    finalState <- Signal.get stateSig
                    finalState `shouldEqual` "2"
                Nothing ->
                    fail "node wasn't spawned"

            pure unit

    describe "bar" $ do
        pure unit