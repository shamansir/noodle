module Noodle.Fn2.Flow
  ( Input(..)
  , Output(..)
  , InputId, OutputId
  , iToSProxy, oToSProxy
  , inputToString, outputToString
  , toInput, toOutput
  , inputId, outputId
--   , inputIdFromString, outputIdFromString
  , inputIdToString, outputIdToString
  , keysToInputs, keysToOutputs
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.List (List)
-- import Data.List (map) as List
-- import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Unsafe.Coerce (unsafeCoerce)


import Prim.RowList as RL
import Record.Extra (keys, class Keys) as Record


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


derive newtype instance eqInputId :: Eq InputId
derive newtype instance ordInputId :: Ord InputId
derive newtype instance showInputId :: Show InputId
-- derive instance newtypeInputId :: Newtype InputId _

derive newtype instance eqOutputId :: Eq OutputId
derive newtype instance ordOutputId :: Ord OutputId
derive newtype instance showOutputId :: Show OutputId
-- derive instance newtypeOutputId :: Newtype OutputId _


instance IsSymbol i => Show (Input i) where
    show _ = "aa"


{-
inputIdFromString :: String -> InputId
inputIdFromString = InputId


outputIdFromString :: String -> OutputId
outputIdFromString = OutputId
-}


toInput :: forall t isym. IsSymbol isym => t isym -> Input isym
toInput _ = Input


toOutput :: forall t osym . IsSymbol osym => t osym -> Output osym
toOutput _ = Output



iToSProxy :: forall i. IsSymbol i => Input i -> SProxy i
iToSProxy _ = SProxy


oToSProxy :: forall o. IsSymbol o => Input o -> SProxy o
oToSProxy _ = SProxy


-- inputId :: forall i. IsSymbol i => Input i -> String
-- inputId = reflectSymbol


inputId :: forall i. IsSymbol i => Input i -> InputId
inputId = InputId <<< reflectSymbol


-- inputId' :: forall i. Input i -> InputId
-- inputId' input =
--     inputId (unsafeCoerce input)
    -- InputId <<< unsafeCoerce <<< reflectSymbol <<< unsafeCoerce


-- outputId :: forall o. IsSymbol o => Output o -> String
-- outputId = reflectSymbol



outputId :: forall o. IsSymbol o => Output o -> OutputId
outputId = OutputId <<< reflectSymbol



inputIdToString :: InputId -> String
inputIdToString (InputId iid) = iid


outputIdToString :: OutputId -> String
outputIdToString (OutputId oid) = oid



-- inputToString :: (forall i. IsSymbol i => Input i) -> String
inputToString :: forall i. IsSymbol i => Input i -> String
inputToString input = reflectSymbol input


-- inputToString' :: (forall i. IsSymbol i => Input i) -> String
-- inputToString' input = (inputToString :: (IsSymbol i => Input i) -> String) (input :: forall i. IsSymbol i => Input i)


-- outputToString :: (forall o. IsSymbol o => Output o) -> String
outputToString :: forall o. IsSymbol o => Output o -> String
outputToString output = reflectSymbol output


-- unsafeInputToString :: forall i. Input i -> String
-- unsafeInputToString input =
--     unsafeCoerce (reflectSymbol (input :: IsSymbol i => Input i))


keysToInputs :: forall w is rl. RL.RowToList is rl => Record.Keys rl => w is -> List InputId
keysToInputs = Record.keys >>> (<$>) InputId


keysToOutputs :: forall w os rl. RL.RowToList os rl => Record.Keys rl => w os -> List OutputId
keysToOutputs = Record.keys >>> (<$>) OutputId