module Blessed.Core.EndStyle where

import Type.Row (type (+))
import Type.Row as R
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core as C
import Blessed.Core.Color (Color)
import Blessed.Core.Border (Border, BorderOption)
import Blessed.Core.Border as Border


type EndStyleRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    , border :: Array (BorderOption ())
    , scrollbar :: Array (BorderOption ())
    )
type EndStyle = Record (EndStyleRow ())


type Evaluated =
    ( fg :: Int
    , bg :: Int
    , border :: Record Border.Evaluated
    , scrollbar :: Record Border.Evaluated
    )


type EndStyleOption (r :: Row Type)
    = C.SoleOption (EndStyleRow + r)


eStyleOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (EndStyleRow + r) => Proxy sym -> a -> EndStyleOption r
eStyleOption = C.onlyOption


fg ∷ forall r. Color -> EndStyleOption ( fg :: Color | r )
fg = eStyleOption ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> EndStyleOption ( bg :: Color | r )
bg = eStyleOption ( Proxy :: Proxy "bg" )


border ∷ forall r. Array (BorderOption ()) -> EndStyleOption ( border :: Array (BorderOption ()) | r )
border = eStyleOption ( Proxy :: Proxy "border" )


scrollbar ∷ forall r. Array (BorderOption ()) -> EndStyleOption ( scrollbar :: Array (BorderOption ()) | r )
scrollbar = eStyleOption ( Proxy :: Proxy "scrollbar" )