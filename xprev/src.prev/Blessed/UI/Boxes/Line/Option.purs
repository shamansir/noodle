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


type LineAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) state e


lineOption
    :: forall subj id a r r' sym state e
     . Respresents Line subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> LineAttribute subj id r state e
lineOption = C.option


fg
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Line subj id
    => Color -> LineAttribute subj id ( fg :: Color | r ) state e
fg = lineOption (Proxy :: _ "fg")


bg
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Line subj id
    => Color -> LineAttribute subj id ( bg :: Color | r ) state e
bg = lineOption (Proxy :: _ "bg")


ch
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Line subj id
    => Char -> LineAttribute subj id ( ch :: Char | r ) state e
ch = lineOption (Proxy :: _ "ch")


type_
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Line subj id
    => B.BorderType -> LineAttribute subj id ( type :: B.BorderType | r ) state e
type_ = lineOption (Proxy :: _ "type")


orientation
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Line subj id
    => Orientation -> LineAttribute subj id ( orientation :: Orientation | r ) state e
orientation = lineOption (Proxy :: _ "orientation")
