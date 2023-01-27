module Blessed.UI.Base.Node.Option where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core (Attribute, option) as C



type OptionsRow :: Row Type -> Row Type
type OptionsRow r =
    ( screen :: String
    -- , parent :: Maybe String
    -- , children :: Array String
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)



type NodeAttribute r e = C.Attribute (OptionsRow + r) e


nodeOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> NodeAttribute r e
nodeOption = C.option



screen :: forall r e. String -> NodeAttribute ( screen :: String | r ) e
screen = nodeOption (Proxy :: _ "screen")
