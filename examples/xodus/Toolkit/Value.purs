module Xodus.Toolkit.Value where


import Prelude

import Data.List (List)
import Data.List (length) as List

import Xodus.Dto
import Xodus.Requests
import Xodus.Query as Q


data Value
    = Bang
    | Databases (List Database)
    | SelectDatabase Database
    | Source Database (List EntityType)
    | SelectType EntityType
    | Query Q.Query
    | Result (List Entity)


instance showValue :: Show Value where
    show Bang = "◌"
    show (Databases databases) = (show $ List.length databases) <> " Databases"
    show (SelectDatabase (Database database)) = database.location
    show (Source (Database database) entityTypes) =
        database.location <> ". " <> (show $ List.length entityTypes) <> " Types"
    show (SelectType (EntityType entityType)) = entityType.name
    show (Query query) = show query
    show (Result entities) = (show $ List.length entities) <> " Entities"
