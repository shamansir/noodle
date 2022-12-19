module Noodle.Fn2.Flow where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)


-- type Input (s :: Symbol) = SProxy
-- type Output (s :: Symbol) = SProxy


class Channel a d | d -> a where
    adapt :: d -> Maybe a
    lift :: a -> d


-- data Input (s :: Symbol) = Input
-- data Output (s :: Symbol) = Output

data Input (iid :: Symbol) = Input String
data Output (oid :: Symbol) = Output String



toInput :: forall t isym. IsSymbol isym => t isym -> Input isym -- should be hidden from outer API
toInput = Input <<< reflectSymbol -- unsafeToInput


toOutput :: forall t osym . IsSymbol osym => t osym -> Output osym -- should be hidden from outer API
toOutput = Output <<< reflectSymbol -- unsafeToOutput


-- inputId :: forall i. IsSymbol i => Input i -> String
-- inputId = reflectSymbol


inputId :: forall i. Input i -> String
inputId (Input iid) = iid


-- outputId :: forall o. IsSymbol o => Output o -> String
-- outputId = reflectSymbol


outputId :: forall o. Output o -> String
outputId (Output oid) = oid