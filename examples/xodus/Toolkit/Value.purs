module Xodus.Toolkit.Value where


import Prelude

import Data.Array (length) as Array
import Data.Maybe

import Xodus.Dto
import Xodus.Query as Q


data Value
    = Bang
    | Databases (Array Database)
    | SelectDatabase Database
    | Source Database (Array EntityType)
    | SelectType EntityType
    | SelectOne Entity
    | Query Q.Query
    | Amount Aggregate
    | ToFilter Q.Condition Q.ConditionInfo
    | ToSort Q.Comparison Q.SortInfo
    | Switch Boolean
    | Result (Array Entity)


data Aggregate
    = All
    | Exactly Int


instance showValue :: Show Value where
    show Bang = "◌"
    show (Databases databases) = (show $ Array.length databases) <> " Databases"
    show (SelectDatabase (Database database)) = database.location
    show (Source (Database database) entityTypes) =
        database.location <> ". " <> (show $ Array.length entityTypes) <> " Types"
    show (SelectType (EntityType entityType)) = entityType.name
    show (SelectOne (Entity entity)) = entity.id
    show (Query query) = show query
    show (Amount (Exactly v)) = show v
    show (Amount All) = "All"
    show (ToFilter _ _) = "Filter"
    show (ToSort _ _) = "Sort"
    show (Switch true) = "on"
    show (Switch false) = "off"
    show (Result entities) = (show $ Array.length entities) <> " Entities"
