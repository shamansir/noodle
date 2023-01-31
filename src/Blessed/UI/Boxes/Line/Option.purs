module Blessed.UI.Boxes.Line.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Line)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box


type OptionsRow r =
    ( orientation :: Orientation
    , type :: B.BorderType
    , bg :: Color
    , fg :: Color
    , ch :: Char
    | r
    )
type Options = Record (OptionsRow ())


type LineAttribute subj id r e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) e


lineOption
    :: forall subj id a r r' sym e
     . Respresents Line subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> LineAttribute subj id r e
lineOption = C.option


fg
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Line subj id
    => Color -> LineAttribute subj id ( fg :: Color | r ) e
fg = lineOption (Proxy :: _ "fg")


bg
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Line subj id
    => Color -> LineAttribute subj id ( bg :: Color | r ) e
bg = lineOption (Proxy :: _ "bg")


ch
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Line subj id
    => Char -> LineAttribute subj id ( ch :: Char | r ) e
ch = lineOption (Proxy :: _ "ch")


orientation
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Line subj id
    => Orientation -> LineAttribute subj id ( orientation :: Orientation | r ) e
orientation = lineOption (Proxy :: _ "orientation")
