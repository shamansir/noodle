module Blessed.UI.Boxes.BigText.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Newtype (class Newtype)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, BigText)
import Blessed.Internal.NodeKey (class Respresents)


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


type BigTextAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) state e


bigTextOption
    :: forall subj id a r r' sym state e
     . Respresents BigText subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (Box.OptionsRow + OptionsRow + r)
    => Proxy sym -> a -> BigTextAttribute subj id r state e
bigTextOption = C.option


bdf :: String -> BDFSource
bdf = BDFSource


font
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents BigText subj id
    => BDFSource -> BigTextAttribute subj id ( font :: BDFSource | r ) state e
font = bigTextOption (Proxy :: _ "font")


fontBold
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents BigText subj id
    => BDFSource -> BigTextAttribute subj id ( fontBold :: BDFSource | r ) state e
fontBold = bigTextOption (Proxy :: _ "fontBold")


fch
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents BigText subj id
    => Char -> BigTextAttribute subj id ( fch :: Char | r ) state e
fch = bigTextOption (Proxy :: _ "fch")
