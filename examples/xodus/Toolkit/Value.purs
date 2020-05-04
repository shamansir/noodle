module Xodus.Toolkit.Value where


import Prelude

import Data.List (List)
import Data.List (length) as List

import Xodus.Toolkit.Dto
import Xodus.Toolkit.Requests


data QueryResult
    = HasEntities (List Entity)
    | HasEntityTypes (List EntityType)


data Value
    = Bang
    | Databases (List Database)
    | Source Database
    | Result QueryResult


instance showValue :: Show Value where
    show Bang = "◌"
    show (Databases databases) = (show $ List.length databases) <> " Databases"
    show (Source (Database database)) = show database.location
    show (Result (HasEntities entities)) = (show $ List.length entities) <> " Entities"
    show (Result (HasEntityTypes entityTypes)) = (show $ List.length entityTypes) <> " Entity Types"
