module Blessed.UI.Forms.Form.Option where

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
import Blessed.Internal.BlessedSubj (Subject, Form)
import Blessed.Internal.NodeKey (class Respresents)

import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box


type OptionsRow r =
    ( keys :: Boolean
    , vi :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type FormAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) state e


formOption
    :: forall subj id a r r' sym state e
     . Respresents Form subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> FormAttribute subj id r state e
formOption = C.option


keys
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Form subj id
    => Boolean -> FormAttribute subj id ( keys :: Boolean | r ) state e
keys = formOption (Proxy :: _ "keys")


vi
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Form subj id
    => Boolean -> FormAttribute subj id ( keys :: Boolean | r ) state e
vi = formOption (Proxy :: _ "keys")