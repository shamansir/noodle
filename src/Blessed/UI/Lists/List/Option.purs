module Blessed.UI.Lists.List.Option where

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


import Blessed.UI.Base.Element.Event (Event)
import Blessed.UI.Base.Element.Option (OptionsRow) as Box


type OptionsRow r =
    ( orientation :: Orientation
    , mouse :: Boolean
    , keys :: Boolean
    , vi :: Boolean
    , items :: Array String
    , search :: String -> Effect Unit
    , interactive :: Boolean
    , invertSelected :: Boolean
    , style_selected :: Array (FgBgOption ())
    , style_item :: Array (FgBgOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListAttribute r e = C.Attribute (Box.OptionsRow + OptionsRow + r) e


listOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ListAttribute r e
listOption = C.option


orientation :: forall r e. Orientation -> ListAttribute ( orientation :: Orientation | r ) e
orientation = listOption (Proxy :: _ "orientation")


mouse :: forall r e. Boolean -> ListAttribute ( mouse :: Boolean | r ) e
mouse = listOption (Proxy :: _ "mouse")


keys :: forall r e. Boolean -> ListAttribute ( keys :: Boolean | r ) e
keys = listOption (Proxy :: _ "keys")


vi :: forall r e. Boolean -> ListAttribute ( vi :: Boolean | r ) e
vi = listOption (Proxy :: _ "vi")


items :: forall r e. Array String -> ListAttribute ( items :: Array String | r ) e
items = listOption (Proxy :: _ "items")


-- TODO:
-- search :: forall r e. (String -> Effect Unit) -> ListAttribute ( search :: (String -> Effect Unit) | r ) e
-- search = listOption (Proxy :: _ "search")


interactive :: forall r e. Boolean -> ListAttribute ( interactive :: Boolean | r ) e
interactive = listOption (Proxy :: _ "interactive")


invertSelected :: forall r e. Boolean -> ListAttribute ( invertSelected :: Boolean | r ) e
invertSelected = listOption (Proxy :: _ "invertSelected")


style_selected ∷ forall r e. Array (FgBgOption ()) -> ListAttribute ( style_selected :: Array (FgBgOption ()) | r ) e
style_selected = listOption ( Proxy :: Proxy "style_selected" )


style_item ∷ forall r e. Array (FgBgOption ()) -> ListAttribute ( style_item :: Array (FgBgOption ()) | r ) e
style_item = listOption ( Proxy :: Proxy "style_item" )