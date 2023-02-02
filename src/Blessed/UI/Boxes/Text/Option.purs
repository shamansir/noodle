module Blessed.UI.Boxes.Text.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Align (HAlign)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Text)
import Blessed.Internal.NodeKey (class Respresents)

import Blessed.UI.Base.Element.Option (OptionsRow) as Element


type OptionsRow r =
    ( fill :: Color
    , align :: HAlign
    | r
    )
type Options = Record (OptionsRow ())


type TextAttribute subj id r state e = C.Attribute subj id (Element.OptionsRow + OptionsRow + r) state e


textOption
    :: forall subj id a r r' sym state e
     . Respresents Text subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TextAttribute subj id r state e
textOption = C.option


fill
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Text subj id
    => Color -> TextAttribute subj id ( fill :: Color | r ) state e
fill = textOption (Proxy :: _ "fill")


align
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Text subj id
    => HAlign -> TextAttribute subj id ( align :: HAlign | r ) state e
align = textOption (Proxy :: _ "align")
