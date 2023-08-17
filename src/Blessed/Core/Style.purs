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
import Blessed.Core.EndStyle (EndStyle, EndStyleOption)
import Blessed.Core.EndStyle as EndStyle



type StyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: Array (BorderOption ())
    , scrollbar :: Array (BorderOption ())
    , hover :: Array (EndStyleOption ())
    , focus :: Array (EndStyleOption ())
    , transparent :: Boolean
    )
type Style =
    Record (StyleRow ())


type Evaluated =
    ( fg :: Int
    , bg :: Int
    , border :: Record Border.Evaluated
    , scrollbar :: Record Border.Evaluated
    , hover :: Record EndStyle.Evaluated
    , focus :: Record EndStyle.Evaluated
    , transparent :: Boolean
    )


-- instance EncodeJson (StyleOption r) where
--     encodeJson (StyleOption onlyProp) = encodeJson onlyProp


-- newtype StyleOption (r :: Row Type)
--     = StyleOption (C.SoleOption (StyleRow + r))


-- we have to use `newtype` since we have a loop in the row type

type StyleOption (r :: Row Type)
    = C.SoleOption (StyleRow + r)


styleOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (StyleRow + r) => Proxy sym -> a -> StyleOption r
styleOption = C.onlyOption


default :: Style
default =
    { border : []
    , scrollbar : []
    , hover : []
    , focus : []
    , fg : "none"
    , bg : "none"
    , transparent : false
    }

{- TODO
  style: {
    fg: 'blue',
    bg: 'black',
    bold: true,
    underline: false,
    blink: false,
    inverse: false,
    invisible: false,
    transparent: false,
    border: {
      fg: 'blue',
      bg: 'red'
    },
    scrollbar: {
      bg: 'blue'
      ch: ' '
    },
    scrollbar : true,
    focus: {
      bg: 'red'
    },
    hover: {
      bg: 'red'
    }
  }

  cursor
-}



fg ∷ forall r. Color -> StyleOption ( fg :: Color | r )
fg = styleOption ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> StyleOption ( bg :: Color | r )
bg = styleOption ( Proxy :: Proxy "bg" )


border ∷ forall r. Array (BorderOption ()) -> StyleOption ( border :: Array (BorderOption ()) | r )
border = styleOption ( Proxy :: Proxy "border" )


scrollbar ∷ forall r. Array (BorderOption ()) -> StyleOption ( scrollbar :: Array (BorderOption ()) | r )
scrollbar = styleOption ( Proxy :: Proxy "scrollbar" )


hover ∷ forall r. Array (EndStyleOption ()) -> StyleOption ( hover :: Array (EndStyleOption ()) | r )
hover = styleOption ( Proxy :: Proxy "hover" )


focus ∷ forall r. Array (EndStyleOption ()) -> StyleOption ( focus :: Array (EndStyleOption ()) | r )
focus = styleOption ( Proxy :: Proxy "focus" )


transparent ∷ forall r. Boolean -> StyleOption ( transparent :: Boolean | r )
transparent = styleOption ( Proxy :: Proxy "transparent" )