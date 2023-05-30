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


import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Forms.TextArea.Option (OptionsRow) as TextArea


type OptionsRow r =
    ( secret :: String
    , censor :: String
    | r
    )
type Options = Record (OptionsRow ())


type TextBoxAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + TextArea.OptionsRow + OptionsRow + r) state e


textBoxOption
    :: forall subj id a r r' sym state e
     . Respresents TextBox subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TextBoxAttribute subj id r state e
textBoxOption = C.option


secret
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents TextBox subj id
    => String -> TextBoxAttribute subj id ( secret :: String | r ) state e
secret = textBoxOption (Proxy :: _ "secret")


censor
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents TextBox subj id
    => String -> TextBoxAttribute subj id ( censor :: String | r ) state e
censor = textBoxOption (Proxy :: _ "censor")
