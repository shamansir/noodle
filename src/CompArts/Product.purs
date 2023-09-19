module CompArts.Product where


import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Bifunctor (lmap)

import Data.Argonaut (decodeJson, jsonParser, JsonDecodeError(..))
import Data.Argonaut.Core (Json)

import Affjax.Node as AJ
import Affjax.ResponseFormat (json)
import Foreign.Object as F

jsonPath = "https://resources.jetbrains.com/cai/brand-data/products.json"



decodeAsMap :: String -> Either JsonDecodeError (Map.Map String (Array String))
decodeAsMap str = do
    json <- lmap TypeMismatch $ jsonParser str
    obj <- decodeJson json
    pure $ Map.fromFoldable $ (F.toUnfoldable obj :: Array _)


type Product =
    { name :: String
    , colors :: Array { red :: Int, green :: Int, blue :: Int, hex :: String }
    }


requestProducts :: Aff (Either AJ.Error (AJ.Response Json))
requestProducts =
    AJ.get json jsonPath
    -- pure $ Left "foo"