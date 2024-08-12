module Test.Spec.Node where

import Prelude


import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
-- import Data.SOrder (type (:::), T)
import Data.Repr (class ToRepr, class FromRepr, class HasFallback, wrap, unwrap)
import Data.Repr (wrap, unwrap) as Repr

import Type.Proxy (Proxy(..))

import Control.Monad.State (modify_)
import Control.Monad.Error.Class (class MonadThrow)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (Error)
import Effect.Aff (Aff, throwError, error)

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
-- import Noodle.Node (Node)
-- import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Fn.Shape (Shape(..), type (⟘), type (⟙), IS, OS, I, O, Hot, Cold)
import Noodle.Fn.Shape (reflect, inlets, outlets) as Shape
import Noodle.Id (Inlet(..), Outlet(..)) as Fn
import Noodle.Id (Temperament(..))

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


{-
shouldContain :: forall state is os. String -> d -> Protocol.Tracker state is os -> Aff Unit
shouldContain id val tracker = do
    values <- liftEffect $ Ref.read tracker
    case Map.lookup id values of
        Just otherVal ->
            if val == otherVal then pure unit
            else throwError $ error $ show val <> " /= " <> show otherVal
        Nothing ->
            throwError $ error $ "\"" <> id <> "\" was not found in tracker"
-}

spec :: Spec Unit
spec = do

    describe "shape" $ do

        it "properly instantiates / reflects shape" $ do
            -- let _ = Shape.reflect (Shape :: _ { inlets :: I "foo" Hot ⟘ I "bar" Cold ⟘ IS, outlets :: O "foo" ⟙ O "bar" ⟙ OS })
            let rawShape = Shape.reflect (Shape :: _ (I "foo" Hot ⟘ I "bar" Cold ⟘ IS) (O "foo" ⟙ O "bar" ⟙ OS))
            Shape.inlets rawShape `shouldEqual`
                [ { name : "foo", order : 0, temp : Hot }
                , { name : "bar", order : 1, temp : Cold }
                ]
            Shape.outlets rawShape `shouldEqual`
                [ { name : "foo", order : 0 }
                , { name : "bar", order : 1 }
                ]





{-
sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }
-}