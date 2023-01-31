module Blessed.UI.DataDisplay.Log.Option where


import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Log)
import Blessed.Internal.NodeKey (class Respresents)

import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( scrollback :: Int
    , scrollOnInput :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type LogAttribute subj id r e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) e


logOption
    :: forall subj id a r r' sym e
     . Respresents Log subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> LogAttribute subj id r e
logOption = C.option


scrollback
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Log subj id
    => Int -> LogAttribute subj id ( pad :: Int | r ) e
scrollback = logOption (Proxy :: _ "pad")


scrollbackOnInput
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Log subj id
    => Boolean -> LogAttribute subj id ( scrollbackOnInput :: Boolean | r ) e
scrollbackOnInput = logOption (Proxy :: _ "scrollbackOnInput")
