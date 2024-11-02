module Blessed.UI.Lists.ListBar.Option where

import Prelude (Unit, unit, ($), (<<<), map, (<$>))

import Effect (Effect)

import Type.Row (type (+))
import Prim.Row as R
import Type.Proxy (Proxy(..))

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Profunctor (wrapIso)
import Data.Newtype (class Newtype)
import Data.Tuple as Tuple

import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

import Blessed.Core.Key (Key)
import Blessed.Core.Key (toString) as Key
import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B
import Blessed.Core.EndStyle (EndStyleOption)
import Blessed.Core.EndStyle (Evaluated) as EndStyle
import Blessed.Core.ListStyle (ListStyle, ListStyleOption) as LS

import Blessed.Internal.Core (Attribute, option, optionWithHandlers, HandlerFn) as C
import Blessed.Internal.BlessedSubj (Subject, ListBar)
import Blessed.Internal.NodeKey (class Respresents)
import Blessed.Internal.Emitter as E

import Blessed.UI.Lists.List.Option (OptionsRow) as List
import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Base.Element.Option (OptionsRow) as Box


newtype CommandRaw = CommandRaw { command :: String, eventUID :: String, keys :: Array String }

newtype CommandsRaw = CommandsRaw (Array CommandRaw)

derive instance Newtype CommandsRaw _
derive instance Newtype CommandRaw _
derive newtype instance EncodeJson CommandRaw
derive newtype instance EncodeJson CommandsRaw

-- instance EncodeJson CommandsRaw where
--     encodeJson (CommandRaw array) =
--         encodeJson (encodeJson <> array
        {- CA.encode $ wrapIso CommandsRaw $ CA.array $ wrapIso CommandRaw $ CA.object "Command" $ CAR.record -- "Commands" $ CAR.record
            { command : CA.string
            , eventUID : CA.string
            , keys : CA.array CA.string
            } -}


type OptionsRow r =
    ( items :: Array String
    , commands :: CommandsRaw -- Array (e /\ C.HandlerFn subj id state)
    -- TODO: , commands :: Array (String /\ Blessed)
    , autoCommandKeys :: Boolean
    -- , style_selected :: Array (EndStyleOption ())
    -- , style_item :: Array (EndStyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


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
    => Array (String /\ Array Key /\ C.HandlerFn subj id state)
    -> ListBarAttribute subj id ( commands :: CommandsRaw | r ) state E.BlessedEvent
commands cmds =
    E.toCore <$> C.optionWithHandlers (Proxy :: _ "commands") (CommandsRaw commands_) cmdsEvents
    where toCmdEvent (cmd /\ keys /\ handler) = ListBar.Command cmd keys /\ handler
          cmdsEvents = toCmdEvent <$> cmds
          toCommand triple = CommandRaw
                                { eventUID : E.uniqueId $ Tuple.fst $ toCmdEvent triple
                                , command : Tuple.fst triple
                                , keys : map Key.toString $ Tuple.fst $ Tuple.snd triple
                                }
          commands_ = toCommand <$> cmds