module CompArts.Product where


import Prelude

import Effect (Effect)
import Effect.Aff (Aff, runAff_)

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Bifunctor (lmap)
import Data.Lens (preview)
import Data.Lens.Index (ix)
import Data.Profunctor.Choice (fanin)
import Data.Array as Array
import Data.Maybe (Maybe(..))

import Data.Argonaut (decodeJson, jsonParser, JsonDecodeError(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Prisms (_Array, _Number, _Object)
import Data.Argonaut.Decode.Class (class DecodeJson)

import Noodle.Node2.HoldsNodeState (class IsNodeState)

import Affjax.Node as AJ
import Affjax.ResponseFormat (json)
import Foreign.Object as F


jsonPath = "https://resources.jetbrains.com/cai/brand-data/products.json" :: String



decodeAsMap :: String -> Either JsonDecodeError (Map.Map String (Array String))
decodeAsMap str = do
    json <- lmap TypeMismatch $ jsonParser str
    obj <- decodeJson json
    pure $ Map.fromFoldable $ (F.toUnfoldable obj :: Array _)


-- newtype PaletteItem
--     = PaletteItem { r :: Int, g :: Int, b :: Int, hex :: String }


-- derive newtype instance DecodeJson PaletteItem


type Product =
    { name :: String
    , palette :: Array { r :: Int, g :: Int, b :: Int, hex :: String }
    }


type ProductsShape =
    { all :: F.Object Product
    }


newtype Products = Products (Array Product)


type ProductRequestError = Either AJ.Error JsonDecodeError


type ProductsMap = Map String Product


type ProductsRequestResult = Either ProductRequestError ProductsMap


requestProducts :: Aff ProductsRequestResult
requestProducts =
    -- TODO: use Data.Profunctor.Choice.fanin to join Eithers: Either x a -> Either y a ->  Either (Either x y) a
    --       or : https://pursuit.purescript.org/packages/purescript-either-extra/0.0.4/docs/Data.Either.Extra#v:catLefts
    AJ.get json jsonPath >>= case _ of
        Left ajErr -> pure $ Left $ Left ajErr
        Right jsonResponce ->
            case (decodeJson jsonResponce.body :: Either JsonDecodeError ProductsShape) of
                Left jsonErr -> pure $ Left $ Right jsonErr
                Right decoded ->
                    pure $ Right $ Map.fromFoldable $ (F.toUnfoldable decoded.all :: Array _)


fromMap :: ProductsMap -> Products
fromMap = Map.values >>> Array.fromFoldable >>> Products


instance Show Products where
    show ps = show (count ps) <> " Products"


count :: Products -> Int
count (Products array) = Array.length array


none :: Products
none = Products [] -- derive newtype instance Monoid


instance IsNodeState Products Products where
    default = none
    fromGlobal = Just