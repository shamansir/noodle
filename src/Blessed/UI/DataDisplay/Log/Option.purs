module Blessed.UI.DataDisplay.Log.Option where


import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( scrollback :: Int
    , scrollOnInput :: Boolean
    | r
    )
type Options = Record (OptionsRow ())


type LogAttribute r e = C.Attribute (List.OptionsRow + OptionsRow + r) e


logOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> LogAttribute r e
logOption = C.option


scrollback :: forall r e. Int -> LogAttribute ( pad :: Int | r ) e
scrollback = logOption (Proxy :: _ "pad")


scrollbackOnInput :: forall r e. Boolean -> LogAttribute ( scrollbackOnInput :: Boolean | r ) e
scrollbackOnInput = logOption (Proxy :: _ "scrollbackOnInput")
