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
import Blessed.Core.EndStyle (EndStyleOption)
import Blessed.Core.EndStyle (Evaluated) as EndStyle
import Blessed.Core.ListStyle (ListStyle, ListStyleOption) as LS

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, ListBar)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( items :: Array String
    -- TODO: , commands :: Array (String /\ Blessed)
    , autoCommandKeys :: Boolean
    -- , style_selected :: Array (EndStyleOption ())
    -- , style_item :: Array (EndStyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListBarAttribute subj id r state e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) state e


lbOption
    :: forall subj id a r r' sym state e
     . Respresents ListBar subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ListBarAttribute subj id r state e
lbOption = C.option


items
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListBar subj id
    => Array String -> ListBarAttribute subj id ( items :: Array String | r ) state e
items = lbOption (Proxy :: _ "items")


autoCommandKeys
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListBar subj id
    => Boolean -> ListBarAttribute subj id ( autoCommandKeys :: Boolean | r ) state e
autoCommandKeys = lbOption (Proxy :: _ "autoCommandKeys")
