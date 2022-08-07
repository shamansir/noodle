module Noodle.Toolkit2
  ( Family
  , family
  , Toolkit
  , spawn
  , trySpawn
  , from
  )
  where

import Prelude

import Data.Semigroup (class Semigroup)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)

import Prim.Row (class Cons)
import Prim.RowList (class RowToList)
import Record as Record
import Record.Extra (class Keys)
import Record.Extra as Record


import Noodle.Node (Node)
import Noodle.Node as Node

newtype Toolkit nodes = Toolkit (Record nodes)

newtype Sep (s :: Symbol) = Sep (Array String)


data Family (s :: Symbol) = Family String


derive newtype instance semigroupSep :: Semigroup (Sep s)
derive newtype instance monoidSep :: Monoid (Sep s)

sep :: forall s. String -> Sep s
sep s = Sep [s]


renderSep :: forall s. IsSymbol s => Sep s -> String
renderSep (Sep items) = let sep = reflectSymbol (SProxy :: SProxy s)
                        in joinWith sep items


comeFrom :: forall s. IsSymbol s => SProxy s -> String
comeFrom _ = renderSep (sep "I come from " <> sep "!!!" :: Sep s)


test :: String -> String
test s = reifySymbol s comeFrom



test2 = produceFamily "foo"


family :: forall proxy s. IsSymbol s => proxy s -> Family s
family f = Family $ reflectSymbol f


produceFamily :: forall s. IsSymbol s => String -> Family s
produceFamily s = reifySymbol s (\_ -> (Family s :: Family s))


from :: forall nodes. Record nodes -> Toolkit nodes
from = Toolkit



{-
spawn
    :: forall m proxy l (nodes :: Row Type) (r' ∷ Row Type) state d
     . Functor m
    => IsSymbol l
    => Cons l (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> proxy l
    -> d
    -> m (Node state d)
spawn (Toolkit tk) sym st =
    Record.get sym tk
        # Node.make' st
    -}


spawn
    :: forall m l (nodes :: Row Type) (r' ∷ Row Type) state d
     . Functor m
    => IsSymbol l
    => Cons l (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> Family l
    -> d
    -> m (Node state d)
spawn (Toolkit tk) sym st =
    Record.get sym tk
        # Node.make' st


trySpawn
    :: forall m l (nodes :: Row Type) (r' ∷ Row Type) state d t
     . Functor m
    => Keys t
    => IsSymbol l
    => RowToList nodes t
    => Cons l (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> String
    -> d
    -> m (Maybe (Node state d))
trySpawn (Toolkit tk) s d =
    if List.elem s $ Record.keys tk then
        Just <$> spawn (Toolkit tk) (produceFamily s :: Family l) d
    else pure Nothing



main = do
  log (reifySymbol "runtime" comeFrom)
  log (comeFrom (SProxy :: SProxy "compile"))