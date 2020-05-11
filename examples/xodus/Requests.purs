module Xodus.Requests where

import Prelude

import Data.Array (take, drop, filter, union, sortBy, intersect, fold) as Array
import Data.Either (either)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as J

import Effect.Aff (Aff)

import Affjax (get)
import Affjax.ResponseFormat (json)

import Xodus.Dto
import Xodus.Query


newtype Method = Method String


rootApi :: String
rootApi = "http://localhost:18080/api"


requestValue :: forall a. J.DecodeJson a => Method -> a -> Aff a
requestValue (Method method) default =
    get json (rootApi <> method)
        <#> either (const default)
            (_.body >>> decodeValue)
    where
        decodeValue :: Json -> a
        decodeValue = J.decodeJson >>> either (const default) identity


requestArray :: forall a. J.DecodeJson a => Method -> Aff (Array a)
requestArray method =
    requestValue method []


getDatabases :: Aff (Array Database)
getDatabases =
    requestArray $ Method "/dbs"


getEntities :: Database -> EntityType -> Aff (Array Entity)
getEntities (Database database) (EntityType entityType) =
    requestValue
        (Method $ "/dbs/" <> database.uuid <> "/entities"
                          <> "?id=" <> show entityType.id
                          <> "&offset=" <> show 0
                          <> "&pageSize=" <> show 50)
        { items : ([] :: Array Entity) }
        <#> _.items


getEntityTypes :: Database -> Aff (Array EntityType)
getEntityTypes (Database database) =
    requestArray $ Method $ "/dbs/" <> database.uuid <> "/types"



perform :: Query -> Aff (Array Entity)
perform (Query' database entityTypes selector) = foldSelector selector
    where
    foldSelector All = Array.fold $ getEntities database <$> entityTypes
    foldSelector (AllOf entityType) = getEntities database entityType
    foldSelector (Take amount selector') = Array.take amount <$> foldSelector selector'
    foldSelector (Drop amount selector') = Array.drop amount <$> foldSelector selector'
    foldSelector (Filter (Condition condition) selector')
            = Array.filter condition <$> foldSelector selector'
    foldSelector (Union selectorA selectorB)
            = Array.union <$> foldSelector selectorA <*> foldSelector selectorB
    foldSelector (Intersect selectorA selectorB)
            = Array.intersect <$> foldSelector selectorA <*> foldSelector selectorB
    foldSelector (Sort (Comparison order) selector')
            = Array.sortBy order <$> foldSelector selector'
