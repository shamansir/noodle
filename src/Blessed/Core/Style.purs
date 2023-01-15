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
import Blessed.Core.Border (Border, BorderProperty)
import Blessed.Core.Border as Border




type StyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: (forall br. Array (BorderProperty br))
    , hover :: (forall sr. Array (StyleProperty sr))
    )
type Style =
    Record (StyleRow ())


instance EncodeJson (StyleProperty r) where
    encodeJson (StyleProperty onlyProp) = encodeJson onlyProp


newtype StyleProperty (r :: Row Type)
    = StyleProperty (C.OnlyProperty (StyleRow + r))


-- we have to use `newtype` since we have a loop in the row type


styleProp :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> StyleProperty r
styleProp sym = StyleProperty <<< C.onlyProperty sym


default :: Style
default =
    { border : []
    , hover : []
    , fg : "none"
    , bg : "none"
    }


fg ∷ forall r. Color -> StyleProperty ( fg :: Color | r )
fg = styleProp ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> StyleProperty ( bg :: Color | r )
bg = styleProp ( Proxy :: Proxy "bg" )


border ∷ forall br r. Array (BorderProperty br) -> StyleProperty ( border :: Array (BorderProperty br) | r )
border = unsafeCoerce <<< styleProp ( Proxy :: Proxy "border" )


hover ∷ forall sr r. Array (StyleProperty sr) -> StyleProperty ( hover :: Array (BorderProperty sr) | r )
hover = unsafeCoerce <<< styleProp ( Proxy :: Proxy "hover" )