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

import Xodus.Toolkit.Value


newtype DatabaseDTO =
    DatabaseDTO
        { uuid :: String
        , location :: String
        , encrypted :: Boolean
        , opened :: Boolean
        }

-- [{"uuid":"49f3ca7e-7985-4e09-9389-1cb6d5c2e762","location":"localhost:3666","key":null,"encryptionProvider":null,"encryptionKey":null,"encryptionIV":null,"encrypted":false,"opened":true},{"uuid":"a5ce2ed2-7119-41c0-a2ad-a430280b8724","location":"noodle","key":null,"encryptionProvider":null,"encryptionKey":null,"encryptionIV":null,"encrypted":false,"opened":true}]



derive instance newtypeAppUser :: Newtype DatabaseDTO _

derive newtype instance encodeJsonAppUser :: J.EncodeJson DatabaseDTO
derive newtype instance decodeJsonAppUser :: J.DecodeJson DatabaseDTO


rootApi :: String
rootApi = "http://localhost:18080/api"


getDatabases :: Aff (List Database)
getDatabases =
    get json (rootApi <> "/dbs")
        <#> either (const Nil)
            (_.body
                >>> decodeDatabases
                >>> map unwrap
                >>> map _.location
                >>> map Database)


decodeDatabases :: Json -> List DatabaseDTO
decodeDatabases v =
    J.decodeJson v # either (const Nil) identity
