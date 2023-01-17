module Blessed.Core.FgBg where

import Type.Row (type (+))
import Type.Row as R
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core as C
import Blessed.Core.Color (Color)


type FgBgRow (r :: Row Type) =
    ( fg :: Color
    , bg :: Color
    )
type FgBg = Record (FgBgRow ())


type Evaluated =
    ( fg :: Int
    , bg :: Int
    )


type FgBgOption (r :: Row Type)
    = C.SoleOption (FgBgRow + r)


fgBgOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (FgBgRow + r) => Proxy sym -> a -> FgBgOption r
fgBgOption = C.onlyOption


fg ∷ forall r. Color -> FgBgOption ( fg :: Color | r )
fg = fgBgOption ( Proxy :: Proxy "fg" )


bg ∷ forall r. Color -> FgBgOption ( bg :: Color | r )
bg = fgBgOption ( Proxy :: Proxy "bg" )