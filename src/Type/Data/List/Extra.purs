module Type.Data.List.Extra where

import Type.Data.List (List', Cons', Nil')


type TList :: forall k. k -> Type
type TList = List'


type TCons :: forall k. k -> TList k -> TList k
type TCons = Cons'


type TNil :: forall k. TList k
type TNil = Nil'
