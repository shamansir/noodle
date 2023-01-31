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

import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( mouse :: Boolean
    , keys :: Boolean
    , inputOnFocus :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type TextAreaAttribute subj id r e = C.Attribute subj id (Input.OptionsRow + OptionsRow + r) e


textAreaOption
    :: forall subj id a r r' sym e
     . Respresents TextArea subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TextAreaAttribute subj id r e
textAreaOption = C.option


mouse
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents TextArea subj id
    => Boolean -> TextAreaAttribute subj id ( mouse :: Boolean | r ) e
mouse = textAreaOption (Proxy :: _ "mouse")


keys
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents TextArea subj id
    => Boolean -> TextAreaAttribute subj id ( keys :: Boolean | r ) e
keys = textAreaOption (Proxy :: _ "keys")


inputOnFocus
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents TextArea subj id
    => Boolean -> TextAreaAttribute subj id ( inputOnFocus :: Boolean | r ) e
inputOnFocus = textAreaOption (Proxy :: _ "inputOnFocus")
