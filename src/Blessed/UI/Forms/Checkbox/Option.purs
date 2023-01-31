module Blessed.UI.Forms.Checkbox.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.FgBg (Evaluated) as FgBg

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Checkbox)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( text :: String
    , checked :: Boolean
    , mouse :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type CheckboxAttribute subj id r e = C.Attribute subj id (Input.OptionsRow + OptionsRow + r) e


cbOption
    :: forall subj id a r r' sym e
     . Respresents Checkbox subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> CheckboxAttribute subj id r e
cbOption = C.option


text
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Checkbox subj id
    => String -> CheckboxAttribute subj id ( text :: String | r ) e
text = cbOption (Proxy :: _ "text")


checked
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Checkbox subj id
    => Boolean -> CheckboxAttribute subj id ( checked :: Boolean | r ) e
checked = cbOption (Proxy :: _ "checked")


mouse
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Checkbox subj id
    => Boolean -> CheckboxAttribute subj id ( mouse :: Boolean | r ) e
mouse = cbOption (Proxy :: _ "mouse")
