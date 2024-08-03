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
import Blessed.Core.EndStyle (EndStyleOption)
import Blessed.Core.EndStyle (Evaluated) as EndStyle
import Blessed.Core.ListStyle (ListStyle, ListStyleOption) as LS

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, List)
import Blessed.Internal.NodeKey (class Respresents)


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
    , style :: Array (LS.ListStyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + OptionsRow + r) state e


listOption
    :: forall subj id a r r' sym state e
     . Respresents List subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ListAttribute subj id r state e
listOption = C.option


orientation
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Orientation -> ListAttribute subj id ( orientation :: Orientation | r ) state e
orientation = listOption (Proxy :: _ "orientation")


mouse
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( mouse :: Boolean | r ) state e
mouse = listOption (Proxy :: _ "mouse")


keys
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( keys :: Boolean | r ) state e
keys = listOption (Proxy :: _ "keys")


vi
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( vi :: Boolean | r ) state e
vi = listOption (Proxy :: _ "vi")


items
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Array String -> ListAttribute subj id ( items :: Array String | r ) state e
items = listOption (Proxy :: _ "items")


-- TODO:
{-
search
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => (String -> Effect Unit) -> ListAttribute subj id ( search :: (String -> Effect Unit) | r ) state e
search = listOption (Proxy :: _ "search")
-}


interactive
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( interactive :: Boolean | r ) state e
interactive = listOption (Proxy :: _ "interactive")


invertSelected
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Boolean -> ListAttribute subj id ( invertSelected :: Boolean | r ) state e
invertSelected = listOption (Proxy :: _ "invertSelected")


type StyleAttrubute subj id state e r = ListAttribute subj id ( style :: Array (LS.ListStyleOption ()) | r ) state e


style
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents List subj id
    => Array (LS.ListStyleOption ()) -> StyleAttrubute subj id state e r
style = listOption (Proxy :: _ "style")