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
        , type :: String
        , label :: String
        , typeId :: Int
        , properties ::
            Array
                { name :: String
                , value :: String
                , type ::
                    { clazz :: String
                    , displayName :: String
                    , readonly :: Boolean
                    }
                }
        }


instance eqEntity :: Eq Entity where
    eq (Entity eA) (Entity eB) = eA.id == eB.id -- FIXME: proper comparison


newtype EntityType =
    EntityType
        { id :: Int
        , name :: String
        }


derive instance newtypeDatabase :: Newtype Database _
derive instance newtypeEntity :: Newtype Entity _
derive instance newtypeEntityType :: Newtype EntityType _


derive newtype instance encodeJsonDatabase :: J.EncodeJson Database
derive newtype instance decodeJsonDatabase :: J.DecodeJson Database
derive newtype instance encodeJsonEntity :: J.EncodeJson Entity
derive newtype instance decodeJsonEntity :: J.DecodeJson Entity
derive newtype instance encodeJsonEntityType :: J.EncodeJson EntityType
derive newtype instance decodeJsonEntityType :: J.DecodeJson EntityType
