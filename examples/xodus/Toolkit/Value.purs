module Xodus.Toolkit.Value where


import Prelude

import Data.List (List)
import Data.List (length) as List


data Database = Database String


data Value
    = Bang
    | Databases (List Database)
    | TheDatabase Database


instance showValue :: Show Value where
    show Bang = "◌"
    show (Databases databases) = (show $ List.length databases) <> " Databases"
    show (TheDatabase (Database databaseName)) = show databaseName
