module Blessed.Core.Border where

import Prelude

import Type.Row (type (+))
import Type.Row as R
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Identity (Identity)
import Data.Newtype (class Newtype)

import Data.Bifunctor (lmap, rmap)

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Blessed.Internal.ArgonautCodecExtra as ACX

import Data.Maybe (Maybe(..))

import Blessed.Internal.Core as C
import Blessed.Core.Color (Color)


data BorderType
    = Line
    | Bg


newtype BorderChar = BorderChar Char


derive instance Newtype BorderChar _


instance Show BorderType where
    show Line = "line"
    show Bg = "bg"


instance EncodeJson BorderType where
    encodeJson Line = CA.encode CA.string "line"
    encodeJson Bg = CA.encode CA.string "bg"


instance EncodeJson BorderChar where
    encodeJson (BorderChar ch_) = CA.encode CA.char ch_


instance DecodeJson BorderChar where
    decodeJson = lmap ACX.convertJsonError' <$> map BorderChar <$> CA.decode CA.char


type BorderRow (r :: Row Type) =
    ( type :: BorderType
    , fg :: Color
    , bg :: Color
    , bold :: Boolean
    , underline :: Boolean
    , ch :: BorderChar
    )
type Border = Record (BorderRow ())


type Evaluated =
    ( type :: String
    , fg :: Int
    , bg :: Int
    , bold :: Boolean
    , underline :: Boolean
    , ch :: BorderChar
    )


type BorderOption (r :: Row Type)
    = C.SoleOption (BorderRow + r)


borderOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (BorderRow + r) => Proxy sym -> a -> BorderOption r
borderOption = C.onlyOption


fg ∷ forall r. Color -> BorderOption ( fg :: Color | r )
fg = borderOption ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> BorderOption ( bg :: Color | r )
bg = borderOption ( Proxy :: Proxy "bg" )


type_ ∷ forall r. BorderType -> BorderOption ( type :: BorderType | r )
type_ = borderOption ( Proxy :: Proxy "type" )


bold ∷ forall r. Boolean -> BorderOption ( bold :: Boolean | r )
bold = borderOption ( Proxy :: Proxy "bold" )


underline ∷ forall r. Boolean -> BorderOption ( underline :: Boolean | r )
underline = borderOption ( Proxy :: Proxy "underline" )


ch ∷ forall r. BorderChar -> BorderOption ( ch :: BorderChar | r )
ch = borderOption ( Proxy :: Proxy "ch" )


_line :: BorderType
_line = Line


_bg :: BorderType
_bg = Bg


fill :: Char -> BorderChar
fill = BorderChar
