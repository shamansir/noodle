module Blessed.UI.DataDisplay.Log.Option where


import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( scrollback :: Int
    , scrollOnInput :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type LogAttribute r = C.Attribute (List.OptionsRow + OptionsRow + r) Event


logOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> LogAttribute r
logOption = C.option


scrollback :: forall r. Int -> LogAttribute ( pad :: Int | r )
scrollback = logOption (Proxy :: _ "pad")


scrollbackOnInput :: forall r. Boolean -> LogAttribute ( scrollbackOnInput :: Boolean | r )
scrollbackOnInput = logOption (Proxy :: _ "scrollbackOnInput")
