module Xodus.Toolkit.Requests where

import Prelude

import Data.List
import Data.Newtype (class Newtype, unwrap)
import Data.Either
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Encode as J
import Data.Argonaut.Decode as J

import Effect.Aff (Milliseconds(..), Aff, launchAff_, delay)

import Affjax (get)
import Affjax.ResponseFormat (json)

import Xodus.Toolkit.Dto


newtype Method = Method String


rootApi :: String
rootApi = "http://localhost:18080/api"


requestList :: forall a. J.DecodeJson a => Method -> Aff (List a)
requestList (Method method) =
    get json (rootApi <> method)
        <#> either (const Nil)
            (_.body >>> decodeList)
    where
        decodeList :: Json -> List a
        decodeList = J.decodeJson >>> either (const Nil) identity


getDatabases :: Aff (List Database)
getDatabases =
    requestList $ Method "/dbs"


getEntities :: Database -> Aff (List Entity)
getEntities _ =
    requestList $ Method "/entities"


getEntityTypes :: Database -> Aff (List EntityType)
getEntityTypes (Database database) =
    requestList $ Method $ "/dbs/" <> database.uuid <> "/types"
