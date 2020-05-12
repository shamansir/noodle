module Xodus.Dto where

import Prelude

import Data.Newtype (class Newtype)
import Data.Array (filter, head, concat, nub, groupBy)
import Data.Array.NonEmpty (head) as NE
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Data.Argonaut.Encode (class EncodeJson) as J
import Data.Argonaut.Decode (class DecodeJson) as J


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


type PropertyData = { name :: String, type :: String, value :: String }


allProperties :: Array Entity -> Array { name :: String, type :: String }
allProperties entities =
    getEntityProps <$> entities # concat # nub
    where
        getEntityProps (Entity { properties }) =
            (\{ name, type : type_ } -> { name, type : type_ })
                <$> getPropertyData
                <$> properties


allPropertiesOf :: Entity -> Array PropertyData
allPropertiesOf (Entity { properties }) =
    getPropertyData <$> properties


dataOfProperty :: Entity -> String -> Maybe PropertyData
dataOfProperty (Entity { properties }) propName =
    case
        filter
            (\p -> p.name == propName)
            properties of
        [] -> Nothing
        props -> getPropertyData <$> head props


getPropertyData
    :: forall r0 r1
     . { name :: String, value :: String, type :: { displayName :: String | r1 } | r0 }
    -> PropertyData
getPropertyData { name, value, type : type_ } =
    { name, value, type : type_.displayName }


groupByType :: Array Entity -> Array (String /\ NonEmptyArray Entity)
groupByType entities =
    groupBy compare entities <#> \vals -> getType (NE.head vals) /\ vals
    where
        getType (Entity { type : type_ }) = type_
        compare entityA entityB =
            eq (getType entityA) (getType entityB)
