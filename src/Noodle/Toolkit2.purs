module Noodle.Toolkit2
  ( Toolkit
  , Family
  , family
  , from, toRecord
  , spawn
  , unsafeSpawn
  , toStates, ToState
  )
  where

import Prelude

import Data.Semigroup (class Semigroup)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.List as List
import Data.List (List)
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr, class Foldable)
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))


import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)

import Prim.Row (class Cons, class Lacks, class Nub)
import Prim.RowList (RowList, class RowToList)
import Record as Record
import Record.Extra (class Keys)
import Record.Extra as Record
import Record.Builder (Builder)
import Record.Builder as Builder


import Heterogeneous.Folding as H
import Heterogeneous.Mapping as H

import Noodle.Node (Node)
import Noodle.Node as Node

newtype Toolkit (nodes :: Row Type) = Toolkit (Record nodes)


data Family (s :: Symbol) = Family String


family :: forall proxy s. IsSymbol s => proxy s -> Family s
family f = Family $ reflectSymbol f


produceFamily :: forall s. IsSymbol s => String -> Family s
produceFamily s = reifySymbol s (\_ -> (Family s :: Family s))


from :: forall (nodes :: Row Type). Record nodes -> Toolkit nodes
from = Toolkit


toRecord :: forall (nodes :: Row Type). Toolkit nodes -> Record nodes
toRecord (Toolkit tk) = tk


x = SProxy :: SProxy "x"
y = SProxy :: SProxy "y"
z = SProxy :: SProxy "z"

_unit = SProxy :: SProxy "_unit"


--testBuilder ∷ ∀ (t24 ∷ Row Type) (t38 ∷ Row Type) (t51 ∷ Row Type) (t62 ∷ Type) (t63 ∷ Row Type) (t74 ∷ Symbol -> Type) (t75 ∷ Symbol) (t78 ∷ Row Type) (t85 ∷ Type) (t89 ∷ Type) (t91 ∷ Row Type). Cons t75 Int (()) t24 ⇒ IsSymbol t75 ⇒ Cons t75 Int t24 t38 ⇒ Lacks t75 t24 ⇒ Nub ( y :: Boolean , z :: String | t38 ) t51 ⇒ Lacks t75 t63 ⇒ Cons t75 t62 t63 t51 ⇒ Cons t75 t85 t78 t63 ⇒ Cons t75 String t78 ( z :: t89 | t91 ) ⇒ Show t85 ⇒ Lacks "z" t91 ⇒ Lacks "y" t91 ⇒ t74 t75 → List (t74 t75) → { y ∷ t89 | t91 }
{- testBuilder x list =
    Builder.build
        (Builder.insert (List.index list 0 # Maybe.fromMaybe x) 42
        >>> Builder.insert (List.index list 3 # Maybe.fromMaybe x) 42
        >>> Builder.delete (List.index list 1 # Maybe.fromMaybe x)
        >>> Builder.modify (List.index list 2 # Maybe.fromMaybe x) show) {}

-}


type UnitRec = ( _unit :: Unit )


{-
testBuilder ∷
    ∀
        (r ∷ Row Type)
        (f ∷ Type -> Type)
        (t26 ∷ Symbol -> Type)
        (t27 ∷ Symbol)
        (t28 ∷ Type)
        (t32 ∷ Symbol -> Type)
        (t33 ∷ Symbol)
    . Foldable f
    ⇒ Cons t27 t28 UnitRec UnitRec
    ⇒ Lacks t27 UnitRec
    ⇒ IsSymbol t27
    ⇒ Cons t33 Int () UnitRec
    ⇒ IsSymbol t33
    ⇒ (t26 t27 → t28)
    → t32 t33
    → f (t26 t27)
    → Record UnitRec
testBuilder fn x keys =
    Builder.build
        (keys
            # List.foldr
                (\key bldr -> bldr >>> Builder.insert key (fn key))
                (Builder.insert _unit unit)
        )
        {}
-}


-- testBuilder ∷
--     ∀
--         (r ∷ Row Type)
--         (f ∷ Type -> Type)
--         (unitl ∷ Symbol)
--         (s' ∷ Symbol)
--         (t ∷ Type)
--         (proxy ∷ Symbol -> Type)
--         (s ∷ Symbol)
--         (x ∷ Row Type)
--     . Foldable f
--     ⇒ Cons s' t UnitRec UnitRec
--     ⇒ Lacks s' UnitRec
--     ⇒ IsSymbol unitl
--     ⇒ IsSymbol s'
--     ⇒ Cons s Int () UnitRec
--     ⇒ IsSymbol s
--     ⇒ (forall x (is :: Symbol). proxy is → x)
--     → proxy s
--     -> (forall (is :: Symbol). IsSymbol is => f (proxy is))
--     → Record UnitRec
--     -- → Record (x + UnitRec)
testBuilder fn x keys =
    Builder.build
        (keys
            # List.foldr
                (\key bldr -> bldr >>> Builder.modify key (fn key))
                (Builder.insert _unit unit)
        )
        {}



testModify fn makeKey keys rec =
        (keys
            # List.foldr
                (\key rec -> let mkey = makeKey key in Record.modify mkey (fn mkey) rec)-- rec >>> Builder.modify key (fn key))
                rec
        )


data ToState = ToState


class GetState from state where
    getState :: forall proxy s. IsSymbol s => proxy s -> from -> state


instance toState ::
  (IsSymbol sym, GetState a b) =>
  H.MappingWithIndex ToState (SProxy sym) a b where
    mappingWithIndex ToState prop a = getState prop a


toStates ∷ ∀ (t117 ∷ Type) (t118 ∷ Type). H.HMapWithIndex ToState t117 t118 ⇒ t117 → t118
toStates = H.hmapWithIndex ToState


-- t' :: forall t40 t41 t42 t43 t47 t48 t49. IsSymbol t42 => Cons t42 t43 t41 t40 => RowToList t48 t49 => Keys t49 => t47 t48 -> Record t40 -> Record t40
-- t' rec = testModify ?wh ?wh (Record.keys rec)-- { x : 5, y : "", z : Nothing }

{-
test rec fn =
    Builder.build
        (foldr
            (\key' bld' -> bld' >>> Builder.insert ?wh (fn ?wh $ Record.get ?wh rec))
            (Builder.insert ?wh ?wh)
            (Record.keys rec)
        )
        {} -}


{-
rebuild :: forall wh wh'. (forall s a b. IsSymbol s => Family s -> a -> b) -> Record wh -> Record wh'
rebuild fn rec =
    let
        insertState :: ∀ proxy r1 r2 r3 l a. IsSymbol l ⇒ Lacks l r1 ⇒ Cons l a r1 r2 ⇒ String → Builder (Record r1) (Record r2)
        insertState key' =
            let family_ = (produceFamily key' :: Family l)
            in -- family_ /\ Record.insert family_ (fn family_ $ Record.get family_ rec) rec'
            Builder.insert family_ (fn family_ $ Record.get family_ rec)
        test :: forall row rl. RowToList wh rl => Keys rl => Builder (Record wh) (Record wh')
        test =
            Builder.build ((foldr (\key' bld' -> ?wh) ?wh (Record.keys rec)) :: Builder (Record wh) (Record wh')) ({} :: Record wh')--
    --in foldr (\key' bld' -> bld' >>> insertState key') (flip Builder.build {}) (Record.keys rec)
    -- in Builder.build $ foldr (\key' bld' -> bld' >>> insertState key') ?wh (Record.keys rec)
    in ({} :: Record wh') -- Builder.build $ foldr (\key' bld' -> ?wh) ?wh (Record.keys rec)
-}

{-
--buildStates :: forall (nodes :: Row Type) (nodes' :: Row Type).  Toolkit (nodes :: Row Type) -> Record nodes'
buildStates ∷ ∀ (t88 ∷ Row Type) (t89 ∷ RowList Type). RowToList t88 t89 ⇒ Keys t89 ⇒ (forall a b. a -> b) -> Toolkit t88 → Record ()
buildStates fn (Toolkit tk) =
    let
        insertState :: ∀ proxy r1 r2 l a. IsSymbol l ⇒ Lacks l r1 ⇒ Cons l a r1 r2 ⇒ String → Record r1 → ( Family l /\ Record r2 )
        insertState key rec =
            let family_ = (produceFamily key :: Family l)
            in family_ /\ Record.insert family_ (fn $ Record.get family_ tk) rec
    in foldr (\key rec -> Tuple.snd $ insertState key rec) {} (Record.keys tk)
-}


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
     . IsSymbol l
    => Cons l (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> Family l
    -> d
    -> m (Node state d)
spawn (Toolkit tk) sym st =
    Record.get sym tk
        # Node.make' st


unsafeSpawn
    :: forall m s (nodes :: Row Type) (r' ∷ Row Type) state d ks
     . Keys ks
    => IsSymbol s
    => RowToList nodes ks
    => Cons s (Node.NodeFn state d) r' nodes
    => MonadEffect m
    => Toolkit nodes
    -> String
    -> d
    -> m (Maybe (Family s /\ Node state d))
unsafeSpawn (Toolkit tk) s d =
    if List.elem s $ Record.keys tk then
        let family_ = produceFamily s :: Family s
        in Just <$> ((/\) family_) <$> (spawn (Toolkit tk) family_ d)
    else pure Nothing


-- not needed
{-
trySpawn'
    :: forall m s (nodes :: Row Type) (r ∷ Row Type) state d
     . IsSymbol s
    => Cons s (Node.NodeFn state d) r nodes
    => MonadEffect m
    => Toolkit nodes
    -> String
    -> d
    -> m (Maybe (Family s /\ Node state d))
trySpawn' (Toolkit tk) s d =
    let family_ = produceFamily s :: Family s
    in Just <$> ((/\) family_) <$> (spawn (Toolkit tk) family_ d)
    -- Just <$> spawn (Toolkit tk) (produceFamily s :: Family l) d
-}