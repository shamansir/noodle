module Blessed.Core.Border where

import Prelude

import Type.Row (type (+))
import Type.Row as R
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Identity (Identity)

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

import Data.Maybe (Maybe(..))

import Blessed.Internal.Core as C
import Blessed.Core.Color (Color)


data BorderType
    = Line


instance Show BorderType where
    show Line = "line"


instance EncodeJson BorderType where
    encodeJson Line = CA.encode CA.string "line"


type BorderRow (r :: Row Type) =
    ( type :: BorderType
    , fg :: Color
    , bg :: Color
    )
type Border = Record (BorderRow ())


type BorderProperty (r :: Row Type)
    = C.OnlyProperty (BorderRow + r)


borderProperty :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> BorderProperty r
borderProperty = C.onlyProperty


fg ∷ forall r. Color -> BorderProperty ( fg :: Color | r )
fg = borderProperty ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> BorderProperty ( bg :: Color | r )
bg = borderProperty ( Proxy :: Proxy "bg" )


default :: Border
default =
    { type : Line
    , fg : "none"
    , bg : "none"
    }


line :: BorderType
line = Line
