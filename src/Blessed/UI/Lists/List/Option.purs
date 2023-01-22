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


type ListAttribute r = C.Attribute (Box.OptionsRow + OptionsRow + r) Event


listOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ListAttribute r
listOption = C.option


orientation :: forall r. Orientation -> ListAttribute ( orientation :: Orientation | r )
orientation = listOption (Proxy :: _ "orientation")


mouse :: forall r. Boolean -> ListAttribute ( mouse :: Boolean | r )
mouse = listOption (Proxy :: _ "mouse")


keys :: forall r. Boolean -> ListAttribute ( keys :: Boolean | r )
keys = listOption (Proxy :: _ "keys")


vi :: forall r. Boolean -> ListAttribute ( vi :: Boolean | r )
vi = listOption (Proxy :: _ "vi")


items :: forall r. Array String -> ListAttribute ( items :: Array String | r )
items = listOption (Proxy :: _ "items")


-- TODO:
-- search :: forall r. (String -> Effect Unit) -> ListAttribute ( search :: (String -> Effect Unit) | r )
-- search = listOption (Proxy :: _ "search")


interactive :: forall r. Boolean -> ListAttribute ( interactive :: Boolean | r )
interactive = listOption (Proxy :: _ "interactive")


invertSelected :: forall r. Boolean -> ListAttribute ( invertSelected :: Boolean | r )
invertSelected = listOption (Proxy :: _ "invertSelected")


style_selected ∷ forall r. Array (FgBgOption ()) -> ListAttribute ( style_selected :: Array (FgBgOption ()) | r )
style_selected = listOption ( Proxy :: Proxy "style_selected" )


style_item ∷ forall r. Array (FgBgOption ()) -> ListAttribute ( style_item :: Array (FgBgOption ()) | r )
style_item = listOption ( Proxy :: Proxy "style_item" )