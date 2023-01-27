module Blessed.UI.Lists.ListBar.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B
import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.FgBg (Evaluated) as FgBg

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( items :: Array String
    -- TODO: , commands :: Array (String /\ Blessed)
    , autoCommandKeys :: Boolean
    , style_selected :: Array (FgBgOption ())
    , style_item :: Array (FgBgOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListBarAttribute r e = C.Attribute (List.OptionsRow + OptionsRow + r) e


lbOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ListBarAttribute r e
lbOption = C.option


items :: forall r e. Array String -> ListBarAttribute ( items :: Array String | r ) e
items = lbOption (Proxy :: _ "items")


autoCommandKeys :: forall r e. Boolean -> ListBarAttribute ( autoCommandKeys :: Boolean | r ) e
autoCommandKeys = lbOption (Proxy :: _ "autoCommandKeys")


style_selected :: forall r e. Array (FgBgOption ()) -> ListBarAttribute ( style_selected :: Array (FgBgOption ()) | r ) e
style_selected = lbOption (Proxy :: _ "style_selected")


style_item :: forall r e. Array (FgBgOption ()) -> ListBarAttribute ( style_item :: Array (FgBgOption ()) | r ) e
style_item = lbOption (Proxy :: _ "style_item")
