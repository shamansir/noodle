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
import Noodle.Fn.Shape (Shape(..), type (⟘), type (⟙), IS, OS, I, O, Hot, Cold, Inlets, Outlets)
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


type TestInlets = (I "foo" Hot Int ⟘ I "c" Hot Int ⟘  I "bar" Cold String ⟘ IS) :: Inlets
type TestOutlets = (O "foo" String ⟙ O "bar" Int ⟙ OS) :: Outlets


spec :: Spec Unit
spec = do

    describe "shape" $ do

        it "properly instantiates / reflects shape" $ do
            let
                rawShape =
                    Shape.reflect (Shape :: _ TestInlets TestOutlets)
            Shape.inlets rawShape `shouldEqual`
                [ { name : "foo", order : 0, temp : Hot }
                , { name : "c"  , order : 1, temp : Hot }
                , { name : "bar", order : 2, temp : Cold }
                ]
            Shape.outlets rawShape `shouldEqual`
                [ { name : "foo", order : 0 }
                , { name : "bar", order : 1 }
                ]

        {-
        it "is inlet" $
            let
                _ = Shape.isInlet (Proxy :: _ "a") (Proxy :: _ Int) (Proxy :: _ (I "a" Hot Int))
                -- _ = Shape.isInlet (Proxy :: _ "b") (Proxy :: _ Int) (Proxy :: _ (I "b" Hot String))
                _ = Shape.isInlet (Proxy :: _ "b") (Proxy :: _ String) (Proxy :: _ (I "b" Hot String))
            in pure unit
        -}

        {-
        it "has inlet" $
            let
                _ = Shape.hasInlet (Proxy :: _ "a") (Proxy :: _ Int) (Proxy :: _ (I "a" Hot Int ⟘ IS))
                _ = Shape.hasInlet (Proxy :: _ "a") (Proxy :: _ Int) (Proxy :: _ (I "a" Hot Int ⟘ I "b" Hot String ⟘ IS))
                _ = Shape.hasInlet (Proxy :: _ "a") (Proxy :: _ Int) (Proxy :: _ (I "b" Hot String ⟘ I "a" Hot Int ⟘ IS))
                _ = Shape.hasInlet (Proxy :: _ "b") (Proxy :: _ String) (Proxy :: _ (I "a" Hot Int ⟘ I "b" Hot String ⟘ IS))
                _ = Shape.hasInlet (Proxy :: _ "c") (Proxy :: _ String) (Proxy :: _ TestInlets2)
                _ = Shape.hasInlet (Proxy :: _ "c") (Proxy :: _ Int) (Proxy :: _ TestInlets)
                _ = Shape.hasInlet (Proxy :: _ "a") (Proxy :: _ Int) (Proxy :: _ (I "b" Hot Int ⟘ I "a" Hot Int ⟘ I "c" Hot String ⟘ IS)) :: Unit
                -- _ = Shape.hasInlet (Proxy :: _ "x") (Proxy :: _ Int) (Proxy :: _ TestInlets)
                -- _ = Shape.hasInlet (Proxy :: _ "c") (Proxy :: _ String) (Proxy :: _ IS)
                _ = unit
            in pure unit
        -}

        {-
        it "inlets match" $
            let
                _ = Shape.inletsMatch (Proxy :: _ IS) {}
                -- _ = Shape.inletsMatch (Proxy :: _ (I "a" Hot Int ⟘ IS)) { }
                _ = Shape.inletsMatch (Proxy :: _ (I "a" Hot Int ⟘ IS)) { a : 2 }
                _ = Shape.inletsMatch (Proxy :: _ (I "a" Hot Int ⟘ I "b" Hot String ⟘ IS)) { a : 2, b : "foo" }
                _ = Shape.inletsMatch (Proxy :: _ (I "a" Hot Int ⟘ I "b" Hot String ⟘ IS)) { b : "foo", a : 2 }
                _ = Shape.inletsMatch (Proxy :: _ TestInlets) { foo : 1, bar : "2", c : 1 }
                _ = Shape.inletsMatch (Proxy :: _ TestInlets) { foo : 1, bar : "2", c : 1, x :17 }
                _ = Shape.inletsMatch (Proxy :: _ TestInlets) { foo : 1, c : 1, x :17 }
                _ = Shape.inletsMatch (Proxy :: _ TestInlets) { foo : 1, c : 5 } -- no bar value
                _ = Shape.inletsMatch (Proxy :: _ TestInlets) { foo : 1 }
                -- _ = Shape.inletsMatch (Proxy :: _ TestInlets) { bar : "2", c : 1, foo : 1 }
                -- _ = Shape.inletsMatch (Proxy :: _ TestInlets) { c : 1, bar : "2", foo : 1 }
                -- _ = Shape.inletsMatch (Proxy :: _ TestInlets2) { a : 1, b : 2, c : "a", d : 7 }
                -- _ = Shape.inletsMatch (Proxy :: _ JustTwo) { a : 1, b : "2" }
                -- _ = Shape.inletsMatch (Proxy :: _ JustTwo2) { a : 1, b : "2" }
                -- _ = Shape.inletsMatch (Proxy :: _ JustTwo) { b : "1", a : 2 }
                -- _ = Shape.inletsMatch (Proxy :: _ JustTwo2) { b : "1", a : 2 }
            in pure unit
        -}



{-
sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }
-}