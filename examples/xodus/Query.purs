module Xodus.Query
    ( Query
    , Query'(..) -- FIXME: do not expose
    , Selector(..)
    , Condition(..), Comparison(..), Field
    , make
    ) where


import Prelude

import Data.Array
import Data.Ord (Ordering)

import Xodus.Dto


type Query = Query' Selector


data Query' a = Query' Database (Array EntityType) a


data Field = Field String


data Condition = Condition (Entity -> Boolean)


data Comparison = Comparison (Entity -> Entity -> Ordering)


data Selector
    = All
    | AllOf EntityType
    | Take Int Selector
    | Drop Int Selector
    | Filter Condition Selector
    | Union Selector Selector
    | Intersect Selector Selector
    | Sort Comparison Selector


make :: Database -> (Array EntityType) -> Selector -> Query
make = Query'


instance functorQuery :: Functor Query' where
    map f (Query' database entityTypes value) = Query' database entityTypes $ f value


instance applyQuery :: Apply Query' where
    apply (Query' _ _ f) (Query' database entityTypes value) = Query' database entityTypes $ f value


instance showQuery :: Show a => Show (Query' a) where
    show (Query' _ _ v) = show v


instance showSelector :: Show Selector where
    show All = "A"
    show (AllOf (EntityType { name })) = "AO(" <> name <> ")"
    show (Take n s) = "T(" <> show n <> "," <> show s <> ")"
    show (Drop n s) = "D(" <> show n <> "," <> show s <> ")"
    show (Union sA sB) = "U(" <> show sA <> "," <> show sB <> ")"
    show (Intersect sA sB) = "I(" <> show sA <> "," <> show sB <> ")"
    show (Sort _ s) = "S(" <> show s <> ")"
    show (Filter _ s) = "F(" <> show s <> ")"
