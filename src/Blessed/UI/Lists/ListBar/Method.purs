module Blessed.UI.Lists.ListBar.Method where

import Prelude

import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Data.Codec.Argonaut as CA
import Data.Argonaut.Encode (encodeJson)

import Blessed.Core.Key (Key)
import Blessed.Core.Key (toString) as Key
import Blessed.Internal.Emitter as E
import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Subject, ListBar)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method, cmethod, HandlerFn) as C

import Blessed.UI.Lists.ListBar.Option (Commands(..)) as ListBar
import Blessed.UI.Lists.ListBar.Event (ListBarEvent(..)) as ListBar

{- TODO -}

setCommands
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Array (String /\ Array Key /\ C.HandlerFn subj id state) -> NodeKey subj id -> BlessedOp state m
setCommands cmds nodeKey =
    C.cmethod nodeKey "setCommands" [ encodeJson commands_ ] cmdsEvents
    where toCmdEvent (cmd /\ keys /\ handler) = ListBar.Command cmd keys /\ handler
          cmdsEvents = toCmdEvent <$> cmds
          toCommand triple = { eventUID : E.uniqueId $ Tuple.fst $ toCmdEvent triple, command : Tuple.fst triple, keys : map Key.toString $ Tuple.fst $ Tuple.snd triple }
          commands_ = ListBar.Commands $ toCommand <$> cmds

{-
setItems items:ArrayString
setCommands commands:ArrayStringBlessed
addItem item:String
addItem' item:String callback:Blessed
select offset:Int
removeItem child:Item
move offset:Int
moveLeft offset:Int
moveRight offset:Int
selectTab index:Int
-}