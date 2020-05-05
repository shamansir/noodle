module Xodus.Dto where

import Prelude (class Eq, eq, (==))

import Data.Newtype (class Newtype, unwrap)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Encode as J
import Data.Argonaut.Decode as J


newtype Database =
    Database
        { uuid :: String
        , location :: String
        , encrypted :: Boolean
        , opened :: Boolean
        }


newtype Entity =
    Entity
        { id :: String
        -- , type :: String
        -- , label :: String
        -- , typeId :: Int
        -- , properties :: Array ({ name :: String, value :: String })
        }


instance eqEntity :: Eq Entity where
    eq (Entity eA) (Entity eB) = eA.id == eB.id -- FIXME: proper comparison


newtype EntityType =
    EntityType
        { id :: Int
        , name :: String
        }


-- [{"uuid":"49f3ca7e-7985-4e09-9389-1cb6d5c2e762","location":"localhost:3666","key":null,"encryptionProvider":null,"encryptionKey":null,"encryptionIV":null,"encrypted":false,"opened":true},{"uuid":"a5ce2ed2-7119-41c0-a2ad-a430280b8724","location":"noodle","key":null,"encryptionProvider":null,"encryptionKey":null,"encryptionIV":null,"encrypted":false,"opened":true}]


-- {"items":[{"id":"0-0","type":"User","label":"User[0-0]","typeId":0,"properties":[{"name":"name","type":{"readonly":false,"clazz":"java.lang.String","displayName":"String"},"value":"John"},{"name":"surname","type":{"readonly":false,"clazz":"java.lang.String","displayName":"String"},"value":"Wick"}],"links":[{"name":"inverted","skip":0,"top":100,"totalCount":1,"entities":[{"id":"0-1","name":"inverted","typeId":0,"type":"User","label":"User[0-1]"}]}],"blobs":[]},{"id":"0-1","type":"User","label":"User[0-1]","typeId":0,"properties":[{"name":"name","type":{"readonly":false,"clazz":"java.lang.String","displayName":"String"},"value":"John"},{"name":"surname","type":{"readonly":false,"clazz":"java.lang.String","displayName":"String"},"value":"Strong"}],"links":[],"blobs":[]}],"totalCount":2}


derive instance newtypeDatabase :: Newtype Database _
derive instance newtypeEntity :: Newtype Entity _
derive instance newtypeEntityType :: Newtype EntityType _


derive newtype instance encodeJsonDatabase :: J.EncodeJson Database
derive newtype instance decodeJsonDatabase :: J.DecodeJson Database
derive newtype instance encodeJsonEntity :: J.EncodeJson Entity
derive newtype instance decodeJsonEntity :: J.DecodeJson Entity
derive newtype instance encodeJsonEntityType :: J.EncodeJson EntityType
derive newtype instance decodeJsonEntityType :: J.DecodeJson EntityType
