module Xodus.Toolkit.Value where


import Prelude

import Data.Array (length) as Array

import Xodus.Dto
import Xodus.Query as Q


data Value
    = Bang
    | Databases (Array Database)
    | SelectDatabase Database
    | Source Database (Array EntityType)
    | SelectType EntityType
    | Query Q.Query
    | Result (Array Entity)


instance showValue :: Show Value where
    show Bang = "◌"
    show (Databases databases) = (show $ Array.length databases) <> " Databases"
    show (SelectDatabase (Database database)) = database.location
    show (Source (Database database) entityTypes) =
        database.location <> ". " <> (show $ Array.length entityTypes) <> " Types"
    show (SelectType (EntityType entityType)) = entityType.name
    show (Query query) = show query
    show (Result entities) = (show $ Array.length entities) <> " Entities"
