module Blessed.UI.Forms.TextArea.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, TextArea)
import Blessed.Internal.NodeKey (class Respresents)

import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box


type OptionsRow r =
    ( mouse :: Boolean
    , keys :: Boolean
    , inputOnFocus :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type TextAreaAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) state e


textAreaOption
    :: forall subj id a r r' sym state e
     . Respresents TextArea subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TextAreaAttribute subj id r state e
textAreaOption = C.option


mouse
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents TextArea subj id
    => Boolean -> TextAreaAttribute subj id ( mouse :: Boolean | r ) state e
mouse = textAreaOption (Proxy :: _ "mouse")


keys
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents TextArea subj id
    => Boolean -> TextAreaAttribute subj id ( keys :: Boolean | r ) state e
keys = textAreaOption (Proxy :: _ "keys")


inputOnFocus
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents TextArea subj id
    => Boolean -> TextAreaAttribute subj id ( inputOnFocus :: Boolean | r ) state e
inputOnFocus = textAreaOption (Proxy :: _ "inputOnFocus")
