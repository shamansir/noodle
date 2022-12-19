module Noodle.Fn2.Flow
  ( Input(..)
  , Output(..)
  , toInput
  , toOutput
  , inputToString
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Unsafe.Coerce (unsafeCoerce)


-- type Input (s :: Symbol) = SProxy
-- type Output (s :: Symbol) = SProxy


class Channel a d | d -> a where
    adapt :: d -> Maybe a
    lift :: a -> d


-- data Input (s :: Symbol) = Input
-- data Output (s :: Symbol) = Output

data Input (iid :: Symbol) = Input
data Output (oid :: Symbol) = Output


-- instance isSymbolInput :: IsSymbol (Input iid) where
--     reflectSymbol = reflectSymbol


newtype InputId = InputId String
newtype OutputId = OutputId String


derive newtype instance showInputId :: Show InputId
derive newtype instance showOutputId :: Show OutputId


toInput :: forall t isym. IsSymbol isym => t isym -> Input isym
toInput _ = Input


toOutput :: forall t osym . IsSymbol osym => t osym -> Output osym
toOutput _ = Output


-- inputId :: forall i. IsSymbol i => Input i -> String
-- inputId = reflectSymbol



inputId :: forall i. IsSymbol i => Input i -> InputId
inputId = InputId <<< reflectSymbol


-- inputId' :: forall i. Input i -> InputId
-- inputId' = InputId <<< unsafeCoerce <<< reflectSymbol <<< unsafeCoerce


-- outputId :: forall o. IsSymbol o => Output o -> String
-- outputId = reflectSymbol


outputId :: forall o. IsSymbol o => Output o -> OutputId
outputId = OutputId <<< reflectSymbol


inputIdToString :: InputId -> String
inputIdToString (InputId iid) = iid


outputIdToString :: OutputId -> String
outputIdToString (OutputId oid) = oid


inputToString :: forall i. IsSymbol i => Input i -> String
inputToString input = reflectSymbol input


-- unsafeInputToString :: forall i. Input i -> String
unsafeInputToString input = inputIdToString $ inputId input