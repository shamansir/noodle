module Blessed.Core.Style where

import Prelude

import Type.Row (type (+))
import Type.Row as R
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

import Data.Maybe (Maybe(..))

import Blessed.Internal.Core as C
import Blessed.Core.Color (Color)
import Blessed.Core.Border (Border)
import Blessed.Core.Border as Border

import Data.Identity (Identity)


type StyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: Maybe Border
    )
type Style =
    Record (StyleRow ())


newtype StyleProp (r :: Row Type)
    = StyleProp (C.Prop (StyleRow + r) Identity Unit)


styleProp :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> StyleProp r
styleProp sym = StyleProp <<< C.prop sym



default :: Style
default =
    { border : Nothing
    , fg : "none"
    , bg : "none"
    }


fg ∷ forall r. Color -> StyleProp ( fg :: Color | r )
fg = styleProp ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> StyleProp ( bg :: Color | r )
bg = styleProp ( Proxy :: Proxy "bg" )


type PropJson = { name :: String, value :: Json }


propCodec :: CA.JsonCodec PropJson
propCodec =
    CA.object "PropJson"
        (CAR.record
            { name : CA.string
            , value : CA.json
            }
        )


instance EncodeJson (StyleProp r) where
    encodeJson (StyleProp prop)
        = case C.isProp prop of
            Just (name /\ value) -> CA.encode propCodec { name, value }
            Nothing -> CA.encode CA.null unit
