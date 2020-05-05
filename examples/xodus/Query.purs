module Xodus.Query
    ( Query
    , Query'(..) -- FIXME: do not expose
    , Selector(..)
    , Condition(..), Comparison(..), Field
    , make
    ) where


import Prelude (class Show, class Functor, ($))

import Data.List
import Data.Ord (Ordering)

import Xodus.Dto


type Query = Query' Selector


data Query' a = Query' Database (List EntityType) a


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


make :: Database -> (List EntityType) -> Selector -> Query
make = Query'


instance functorQuery :: Functor Query' where
    map f (Query' database entityTypes value) = Query' database entityTypes $ f value


instance showQuery :: Show (Query' a) where
    show _ = "query"
