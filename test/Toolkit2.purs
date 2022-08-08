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
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Noodle.Channel as Ch
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit

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
    , concat :: Node.NodeFn Int String
    )


-- _sum = Proxy :: Proxy "sum"
_sum = T.family Proxy :: T.Family "sum"
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


spawnConcat ∷ ∀ m. Functor m ⇒ MonadEffect m ⇒ String → m (Node Int String)
spawnConcat = T.spawn toolkit _concat

-- fails:
{-
spawnFoo ∷ ∀ state d m. Functor m ⇒ MonadEffect m ⇒ String → m (Node state d)
spawnFoo = T.spawn toolkit _foo
-}



trySpawn ∷
    forall (m ∷ Type -> Type) (s ∷ Symbol) (r ∷ Row Type) state d
     . IsSymbol s
    => Cons s (Node.NodeFn state d) r Nodes
    => MonadEffect m
    => String
    -> d
    -> m (Maybe ((T.Family s) /\ (Node state d)))
trySpawn = T.trySpawn toolkit


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