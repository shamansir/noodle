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


type BigTextAttribute r e = C.Attribute (Box.OptionsRow + OptionsRow + r) e


bigTextOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> BigTextAttribute r e
bigTextOption = C.option


bdf :: String -> BDFSource
bdf = BDFSource


font :: forall r e. BDFSource -> BigTextAttribute ( font :: BDFSource | r ) e
font = bigTextOption (Proxy :: _ "font")


fontBold :: forall r e. BDFSource -> BigTextAttribute ( fontBold :: BDFSource | r ) e
fontBold = bigTextOption (Proxy :: _ "fontBold")


fch :: forall r e. Char -> BigTextAttribute ( fch :: Char | r ) e
fch = bigTextOption (Proxy :: _ "fch")
