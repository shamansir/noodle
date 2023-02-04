module Blessed.Core.ListStyle where

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
import Blessed.Core.FgBg (FgBg, FgBgOption)
import Blessed.Core.FgBg as FgBg



type ListStyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: Array (BorderOption ())
    , scrollbar :: Array (BorderOption ())
    , hover :: Array (FgBgOption ())
    , focus :: Array (FgBgOption ())
    , item :: Array (FgBgOption ())
    , selected :: Array (FgBgOption ())
    )
type ListStyle =
    Record (ListStyleRow ())


type Evaluated =
    ( fg :: Int
    , bg :: Int
    , border :: Record Border.Evaluated
    , scrollbar :: Record Border.Evaluated
    , hover :: Record FgBg.Evaluated
    , focus :: Record FgBg.Evaluated
    , item :: Record FgBg.Evaluated
    , selected :: Record FgBg.Evaluated
    )


type ListStyleOption (r :: Row Type)
    = C.SoleOption (ListStyleRow + r)


listStyleOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (ListStyleRow + r) => Proxy sym -> a -> ListStyleOption r
listStyleOption = C.onlyOption


default :: ListStyle
default =
    { border : []
    , scrollbar : []
    , hover : []
    , focus : []
    , fg : "none"
    , bg : "none"
    , item : []
    , selected : []
    }


fg ∷ forall r. Color -> ListStyleOption ( fg :: Color | r )
fg = listStyleOption ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> ListStyleOption ( bg :: Color | r )
bg = listStyleOption ( Proxy :: Proxy "bg" )


border ∷ forall r. Array (BorderOption ()) -> ListStyleOption ( border :: Array (BorderOption ()) | r )
border = listStyleOption ( Proxy :: Proxy "border" )


scrollbar ∷ forall r. Array (BorderOption ()) -> ListStyleOption ( scrollbar :: Array (BorderOption ()) | r )
scrollbar = listStyleOption ( Proxy :: Proxy "scrollbar" )


hover ∷ forall r. Array (FgBgOption ()) -> ListStyleOption ( hover :: Array (FgBgOption ()) | r )
hover = listStyleOption ( Proxy :: Proxy "hover" )


focus ∷ forall r. Array (FgBgOption ()) -> ListStyleOption ( focus :: Array (FgBgOption ()) | r )
focus = listStyleOption ( Proxy :: Proxy "focus" )


item ∷ forall r. Array (FgBgOption ()) -> ListStyleOption ( item :: Array (FgBgOption ()) | r )
item = listStyleOption ( Proxy :: Proxy "item" )


selected ∷ forall r. Array (FgBgOption ()) -> ListStyleOption ( selected :: Array (FgBgOption ()) | r )
selected = listStyleOption ( Proxy :: Proxy "selected" )