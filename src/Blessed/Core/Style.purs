module Blessed.Core.Style where

import Prelude

import Type.Row (type (+))
import Type.Row as R
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Identity (Identity)
import Unsafe.Coerce (unsafeCoerce)
import Data.Newtype (class Newtype)

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

import Data.Maybe (Maybe(..))

import Blessed.Internal.Core as C
import Blessed.Core.Color (Color)
import Blessed.Core.Border (Border, BorderOption)
import Blessed.Core.Border as Border




type StyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: (forall br. Array (BorderOption br))
    , hover :: (forall sr. Array (StyleOption sr))
    )
type Style =
    Record (StyleRow ())


instance EncodeJson (StyleOption r) where
    encodeJson (StyleOption onlyProp) = encodeJson onlyProp


newtype StyleOption (r :: Row Type)
    = StyleOption (C.SoleOption (StyleRow + r))


-- we have to use `newtype` since we have a loop in the row type


styleOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> StyleOption r
styleOption sym = StyleOption <<< C.onlyOption sym


default :: Style
default =
    { border : []
    , hover : []
    , fg : "none"
    , bg : "none"
    }


fg ∷ forall r. Color -> StyleOption ( fg :: Color | r )
fg = styleOption ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> StyleOption ( bg :: Color | r )
bg = styleOption ( Proxy :: Proxy "bg" )


border ∷ forall br r. Array (BorderOption br) -> StyleOption ( border :: Array (BorderOption br) | r )
border = unsafeCoerce <<< styleOption ( Proxy :: Proxy "border" )


hover ∷ forall sr r. Array (StyleOption sr) -> StyleOption ( hover :: Array (BorderOption sr) | r )
hover = unsafeCoerce <<< styleOption ( Proxy :: Proxy "hover" )