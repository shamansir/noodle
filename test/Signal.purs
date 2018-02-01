module Test.Signal
  ( expect'
  ) where

import Prelude
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable, toUnfoldable)
import Signal (Signal, (~>), runSignal)

expect' :: forall e a. Eq a => Show a => Signal a -> Array a -> Aff (ref :: REF | e) Unit
expect' sig vals = makeAff \resolve -> do
  remaining <- newRef vals
  let getNext val = do
        nextValArray <- readRef remaining
        let nextVals = fromFoldable nextValArray
        case nextVals of
          Cons x xs -> do
            if x /= val then resolve $ Left $ error $ "expected " <> show x <> " but got " <> show val
              else case xs of
                Nil -> resolve $ Right unit
                _ -> writeRef remaining (toUnfoldable xs)
          Nil -> resolve $ Left $ error "unexpected emptiness"
  runSignal $ sig ~> getNext
  pure nonCanceler
