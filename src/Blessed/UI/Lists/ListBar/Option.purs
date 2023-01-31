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
import Blessed.Internal.BlessedSubj (Subject, ListBar)
import Blessed.Internal.NodeKey (class Respresents)


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


type ListBarAttribute subj id r e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) e


lbOption
    :: forall subj id a r r' sym e
     . Respresents ListBar subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ListBarAttribute subj id r e
lbOption = C.option


items
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents ListBar subj id
    => Array String -> ListBarAttribute subj id ( items :: Array String | r ) e
items = lbOption (Proxy :: _ "items")


autoCommandKeys
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents ListBar subj id
    => Boolean -> ListBarAttribute subj id ( autoCommandKeys :: Boolean | r ) e
autoCommandKeys = lbOption (Proxy :: _ "autoCommandKeys")


style_selected
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents ListBar subj id
    => Array (FgBgOption ()) -> ListBarAttribute subj id ( style_selected :: Array (FgBgOption ()) | r ) e
style_selected = lbOption (Proxy :: _ "style_selected")


style_item
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents ListBar subj id
    => Array (FgBgOption ()) -> ListBarAttribute subj id ( style_item :: Array (FgBgOption ()) | r ) e
style_item = lbOption (Proxy :: _ "style_item")
