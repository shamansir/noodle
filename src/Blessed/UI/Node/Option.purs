module Blessed.UI.Node.Option where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.UI.Screen.Event (Event)



type OptionsRow :: Row Type -> Row Type
type OptionsRow r =
    ( screen :: String
    -- , parent :: Maybe String
    -- , children :: Array String
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)



type NodeAttribute r = C.Attribute (OptionsRow + r) Event


nodeOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> NodeAttribute r
nodeOption = C.option



screen :: forall r. String -> NodeAttribute ( screen :: String | r)
screen = nodeOption (Proxy :: _ "screen")
