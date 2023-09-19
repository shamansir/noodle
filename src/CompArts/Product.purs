module CompArts.Product where


import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Bifunctor (lmap)
import Data.Lens (preview)
import Data.Lens.Index (ix)

import Data.Argonaut (decodeJson, jsonParser, JsonDecodeError(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Prisms (_Array, _Number, _Object)
import Data.Argonaut.Decode.Class (class DecodeJson)

import Affjax.Node as AJ
import Affjax.ResponseFormat (json)
import Foreign.Object as F


jsonPath = "https://resources.jetbrains.com/cai/brand-data/products.json"



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


type Products =
    { all :: F.Object Product
    }


requestProducts :: Aff (Either (Either AJ.Error JsonDecodeError) (Map.Map String Product))
requestProducts =
    AJ.get json jsonPath >>= case _ of
        Left ajErr -> pure $ Left $ Left ajErr
        Right jsonResponce ->
            case (decodeJson jsonResponce.body :: Either JsonDecodeError Products) of
                Left jsonErr -> pure $ Left $ Right jsonErr
                Right decoded ->
                    pure $ Right $ Map.fromFoldable $ (F.toUnfoldable decoded.all :: Array _)