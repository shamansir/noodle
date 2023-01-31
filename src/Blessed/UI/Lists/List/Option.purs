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
import Blessed.Internal.BlessedSubj (Subject, List)
import Blessed.Internal.NodeKey (class Respresents)


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


type ListAttribute subj id r e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) e


listOption
    :: forall subj id a r r' sym e
     . Respresents List subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ListAttribute subj id r e
listOption = C.option


orientation
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Orientation -> ListAttribute subj id ( orientation :: Orientation | r ) e
orientation = listOption (Proxy :: _ "orientation")


mouse
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( mouse :: Boolean | r ) e
mouse = listOption (Proxy :: _ "mouse")


keys
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( keys :: Boolean | r ) e
keys = listOption (Proxy :: _ "keys")


vi
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( vi :: Boolean | r ) e
vi = listOption (Proxy :: _ "vi")


items
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Array String -> ListAttribute subj id ( items :: Array String | r ) e
items = listOption (Proxy :: _ "items")


-- TODO:
{-
search
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => (String -> Effect Unit) -> ListAttribute subj id ( search :: (String -> Effect Unit) | r ) e
search = listOption (Proxy :: _ "search")
-}


interactive
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( interactive :: Boolean | r ) e
interactive = listOption (Proxy :: _ "interactive")


invertSelected
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( invertSelected :: Boolean | r ) e
invertSelected = listOption (Proxy :: _ "invertSelected")


style_selected
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Array (FgBgOption ()) -> ListAttribute subj id ( style_selected :: Array (FgBgOption ()) | r ) e
style_selected = listOption ( Proxy :: Proxy "style_selected" )


style_item
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents List subj id
    => Array (FgBgOption ()) -> ListAttribute subj id ( style_item :: Array (FgBgOption ()) | r ) e
style_item = listOption ( Proxy :: Proxy "style_item" )