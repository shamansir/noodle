module Blessed.UI.Boxes.BigText.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Newtype (class Newtype)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Base.Element.Event (Event)
import Blessed.UI.Base.Element.Option (OptionsRow) as Box


newtype BDFSource = BDFSource String

derive instance Newtype BDFSource _
instance EncodeJson BDFSource where encodeJson (BDFSource src) = encodeJson src


type OptionsRow r =
    ( font :: BDFSource
    , fontBold :: BDFSource
    , fch :: Char
    | r
    )
type Options = Record (OptionsRow ())


type BigTextAttribute r = C.Attribute (Box.OptionsRow + OptionsRow + r) Event


bigTextOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> BigTextAttribute r
bigTextOption = C.option


bdf :: String -> BDFSource
bdf = BDFSource


font :: forall r. BDFSource -> BigTextAttribute ( font :: BDFSource | r )
font = bigTextOption (Proxy :: _ "font")


fontBold :: forall r. BDFSource -> BigTextAttribute ( fontBold :: BDFSource | r )
fontBold = bigTextOption (Proxy :: _ "fontBold")


fch :: forall r. Char -> BigTextAttribute ( fch :: Char | r )
fch = bigTextOption (Proxy :: _ "fch")
