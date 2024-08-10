module Test.Spec.Repr where

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

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)
import Data.Symbol (reflectSymbol)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
-- import Noodle.Node (Node)
-- import Noodle.Node as Node
import Noodle.Fn (Fn)
import Noodle.Fn as Fn
import Noodle.Fn.Process as Fn
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Protocol as Protocol
import Noodle.Id (Inlet(..), Outlet(..)) as Fn
import Noodle.Id (Temperament(..))
import Noodle.Fn.RawToRec as RR

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

data Refl
    = Refl Int


derive instance Eq Refl


instance Show Refl where show = case _ of Refl n -> show n
instance HasFallback Refl where fallback = Refl 0
instance ToRepr Int Refl where toRepr = Just <<< Repr.wrap <<< Refl
instance FromRepr Refl Int where fromRepr = Repr.unwrap >>> case _ of Refl n -> Just n


spec :: Spec Unit
spec = do

    describe "creating maps from reprs" $ do

        it "converting to repr works" $ do
            let recMap = RR.fromRec reflectSymbol { a : 5, b : 3 }
            Map.lookup "a" recMap `shouldEqual` (Just $ Refl 5)
            Map.lookup "b" recMap `shouldEqual` (Just $ Refl 3)

        {-
        itOnly "converting from repr works" $ do
            let recMap = RR.toRec reflectSymbol { a : 5, b : 3 }
            Map.lookup "a" recMap `shouldEqual` (Just $ Refl 5)
            Map.lookup "b" recMap `shouldEqual` (Just $ Refl 3)
        -}


{-
sumOrders :: Fn.Orders _ _
sumOrders =
    { inputs : Proxy :: _ ( "a" ::: "b" ::: T )
    , outputs : Proxy :: _ ( "sum" ::: T )
    }
-}