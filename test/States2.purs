module Test.States2 where

import Prelude

import Effect.Aff (Aff)

import Data.Array as Array
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


type NodeStates =
    ( foo :: String
    , bar :: Int
    , buz :: Array Int
    )


defaults :: Record NodeStates
defaults =
    { foo : "aaa"
    , bar : 0
    , buz : [ 0 ]
    }


getDefaultState
   :: forall proxy l a r'
    . IsSymbol l
   => Cons l a r' NodeStates
   => proxy l
   -> a
getDefaultState =
    getDefaultState' defaults


getDefaultState'
   :: forall proxy l a r' x -- (x :: Row Type)
    . IsSymbol l
   => Cons l a r' x
   => Record x
   -> proxy l
   -> a
getDefaultState' rec label =
    Record.get label rec



{- =========== -}


data NodeState a = NodeState a (Array a)


state :: forall a. a -> NodeState a
state a = NodeState a []


type NodeStatesP =
    ( foo :: NodeState String
    , bar :: NodeState Int
    , buz :: NodeState (Array Int)
    )


states :: Record NodeStatesP
states =
    { foo : state "aaa"
    , bar : state 0
    , buz : state [0]
    }


getDefault :: forall a. NodeState a -> a
getDefault (NodeState default _) = default


getInstance :: forall a. NodeState a -> Int -> Maybe a
getInstance (NodeState _ instances) = Array.index instances


getDefaultStateP
   :: forall proxy l a r'
    . IsSymbol l
   => Cons l (NodeState a) r' NodeStatesP
   => proxy l
   -> a
getDefaultStateP =
    getDefaultStateP' states

getDefaultStateP'
   :: forall proxy l a r' x -- (x :: Row Type)
    . IsSymbol l
   => Cons l (NodeState a) r' x
   => Record x
   -> proxy l
   -> a
getDefaultStateP' rec label = Record.get label rec # getDefault


getInstanceStateP
   :: forall proxy l a r'
    . IsSymbol l
   => Cons l (NodeState a) r' NodeStatesP
   => proxy l
   -> Int
   -> Maybe a
getInstanceStateP =
    getInstanceStateP' states


getInstanceStateP'
   :: forall proxy l a r' x -- (x :: Row Type)
    . IsSymbol l
   => Cons l (NodeState a) r' x
   => Record x
   -> proxy l
   -> Int
   -> Maybe a
getInstanceStateP' rec label idx = Record.get label rec # flip getInstance idx


getFooStateP :: String
getFooStateP = getDefaultStateP _foo


getFooInstanceStateP :: Int -> Maybe String
getFooInstanceStateP n = getInstanceStateM _foo n

{- =========== -}

--type NodeStatesM :: forall k. (Type -> k) -> Row k
type NodeStatesM m =
    ( foo :: m String
    , bar :: m Int
    , buz :: m (Array Int)
    )


newtype One a = One a

wrap :: forall a. a -> One a
wrap = One


unwrap :: forall a. One a -> a
unwrap (One a) = a


-- myPatches :: forall r. MyPatches r
defaultsM :: Record (NodeStatesM One)
defaultsM =
    { foo : wrap "aaa"
    , bar : wrap 0
    , buz : wrap [ 0 ]
    }


instancesM :: Record (NodeStatesM Array)
instancesM =
    { foo : [ "aaa", "bbb" ]
    , bar : [ 0, 2, 14 ]
    , buz : [ [ 0 ]  ]
    }


-- tryGet :: Int
{- getDefaultStateM
   :: forall proxy l a (r :: # Type)
    . IsSymbol l
   => Cons l (One a) (NodeStatesM One) (NodeStatesM One)
   => proxy l
   -> a -}
-- getDefaultStateM ∷ ∀ proxy l a r'. IsSymbol l ⇒ Cons l (One a) r' (NodeStatesM One) ⇒ proxy l → a
--getDefaultStateM ∷ ∀ proxy l a r'. IsSymbol l ⇒ Cons l (One a) r' (NodeStatesM One) ⇒ proxy l → a
getDefaultStateM ∷ ∀ (proxy ∷ Symbol -> Type) (l ∷ Symbol) (a ∷ Type) (r' ∷ Row Type). IsSymbol l ⇒ Cons l (One a) r' (NodeStatesM One) ⇒ proxy l → a
getDefaultStateM label =
    Record.get label defaultsM # unwrap
    -- getDefaultStateM' (unsafeCoerce $ defaultsM :: _) label
    -- FIXME: getDefaultStateM' defaultsM label
    -- getDefaultStateM' defaultsM _foo


getDefaultStateM'
   :: forall proxy l a r' (x :: forall k. (Type -> k) -> Row k)
    . IsSymbol l
   => Cons l (One a) r' (x One)
   => Record (x One)
   -> proxy l
   -> a
getDefaultStateM' rec label =
    Record.get label rec # unwrap


getInstanceStateM
   :: forall proxy l a r'
    . IsSymbol l
   => Cons l (Array a) r' (NodeStatesM Array)
   => proxy l
   -> Int
   -> Maybe a
getInstanceStateM label idx =
    -- getInstanceState' ?wh
    --getInstanceState' (?wh :: Record (_ Array)) label idx
    Record.get label instancesM # flip Array.index idx


getInstanceStateM'
   :: forall proxy l r' a (x :: forall k. (Type -> k) -> Row k)
    . IsSymbol l
   => Cons l (Array a) r' (x Array)
   => Record (x Array)
   -> proxy l
   -> Int
   -> Maybe a
getInstanceStateM' rec label idx =
    Record.get label rec # flip Array.index idx



{- getInstanceStateM''
   :: forall proxy l a r'
    . IsSymbol l
   => Cons l (Array a) r' (NodeStatesM Array)
   => proxy l
   -> Int
   -> Maybe a
getInstanceStateM'' label idx =
    getInstanceStateM' instancesM label idx
    --getInstanceState' (?wh :: Record (_ Array)) label idx
    --Record.get label instances # flip Array.index idx
-}


_foo = Proxy :: Proxy "foo"


getFooState :: String
getFooState = getDefaultStateM _foo


getFooInstanceState :: Int -> Maybe String
getFooInstanceState n = getInstanceStateM _foo n