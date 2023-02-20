module Blessed.UI.Lists.ListBar.Option where

import Prelude (Unit, unit, ($), (<<<), map)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Profunctor (wrapIso)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)

import Blessed.Core.Key (Key)
import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B
import Blessed.Core.EndStyle (EndStyleOption)
import Blessed.Core.EndStyle (Evaluated) as EndStyle
import Blessed.Core.ListStyle (ListStyle, ListStyleOption) as LS

import Blessed.Internal.Core (Attribute, option, optionWithHandlers, HandlerFn) as C
import Blessed.Internal.BlessedSubj (Subject, ListBar)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Option (OptionsRow) as List
import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Base.Element.Option (OptionsRow) as Box


newtype Commands = Commands Unit

derive instance Newtype Commands _

instance EncodeJson Commands where
    encodeJson cmds = CA.encode (wrapIso Commands $ CA.null) cmds


type OptionsRow r =
    ( items :: Array String
    , commands :: Commands -- Array (e /\ C.HandlerFn subj id state)
    -- TODO: , commands :: Array (String /\ Blessed)
    , autoCommandKeys :: Boolean
    -- , style_selected :: Array (EndStyleOption ())
    -- , style_item :: Array (EndStyleOption ())
    | r
    )
type Options subj id state e = Record (OptionsRow ())


type ListBarAttribute subj id r state e = C.Attribute subj id (Box.OptionsRow + List.OptionsRow + OptionsRow + r) state e


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


commands
    :: forall (subj :: Subject) (id :: Symbol) r state
     . Respresents ListBar subj id
    => Array (String /\ Array Key /\ C.HandlerFn subj id state) -> ListBarAttribute subj id ( commands :: Commands | r ) state ListBar.Event
commands = C.optionWithHandlers (Proxy :: _ "commands") (Commands unit) <<< map toCmdEvent
    where
        toCmdEvent (cmd /\ keys /\ handler) = ListBar.Command cmd keys /\ handler