module Blessed.UI.Forms.TextBox.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, TextBox)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Boxes.Box.Event (Event)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input


type OptionsRow r =
    ( secret :: String
    , censor :: String
    | r
    )
type Options = Record (OptionsRow ())


type TextBoxAttribute subj id r e = C.Attribute subj id (Input.OptionsRow + OptionsRow + r) e


textBoxOption
    :: forall subj id a r r' sym e
     . Respresents TextBox subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TextBoxAttribute subj id r e
textBoxOption = C.option


secret
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents TextBox subj id
    => String -> TextBoxAttribute subj id ( secret :: String | r ) e
secret = textBoxOption (Proxy :: _ "secret")


censor
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents TextBox subj id
    => String -> TextBoxAttribute subj id ( censor :: String | r ) e
censor = textBoxOption (Proxy :: _ "censor")