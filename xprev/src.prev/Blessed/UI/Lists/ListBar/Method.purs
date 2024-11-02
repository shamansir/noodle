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

import Blessed.UI.Lists.ListBar.Option (CommandsRaw(..), CommandRaw(..)) as ListBar
import Blessed.UI.Lists.ListBar.Event (ListBarEvent(..)) as ListBar


{- TODO -}

setItems
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Array (String /\ Array Key /\ C.HandlerFn subj id state) -> NodeKey subj id -> BlessedOp state m
setItems cmds nodeKey =
    C.cmethod nodeKey "setItems" [ encodeJson jsonCommands ] cmdsEvents
            {- [ ( "foo" /\ { foo : "bar" } )
            -- , ( "foo" /\ { foo : "bar" } )
            -- , ( "foo" /\ { foo : "ololo" } )
            -- -}  -- NO ERROR
            {- [ ( "foo" /\ "foo" /\ { foo : "bar" } )
            -- , ( "foo" /\ "bar" /\ { foo : "bar" } )
            -- , ( "foo" /\ "bar" /\ { foo : "ololo" } )
            -- ] -} -- NO ERROR
            -- [ { foo : "bar" } ] -- ERROR
    where toCmdEvent (cmd /\ keys /\ handler) = (E.toCore $ ListBar.Command cmd keys) /\ handler
          cmdsEvents = toCmdEvent <$> cmds
          toJsonArg triple =
                    ( "jsonCmd" /\ -- `jsonCmd` is the hack to make Argonaut properly encode the record
                        { eventUID : E.uniqueId $ Tuple.fst $ toCmdEvent triple
                        , command : Tuple.fst triple
                        , keys : map Key.toString $ Tuple.fst $ Tuple.snd triple
                        }
                    )
          jsonCommands = toJsonArg <$> cmds


addItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => String -> Array Key -> NodeKey subj id -> BlessedOp state m
addItem cmd keys nodeKey =
    --addItemH cmd keys $ const $ const $ pure unit
    C.method nodeKey "addItem"
        [ C.arg CA.string cmd
        , C.arg (CA.array CA.string) (Key.toString <$> keys)
        ]


addItemH
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => String -> Array Key -> C.HandlerFn subj id state -> NodeKey subj id -> BlessedOp state m
addItemH cmd keys handler nodeKey =
    let lbCommandEvt = E.toCore $ ListBar.Command cmd keys
    in C.cmethod nodeKey "addItemH"
        [ encodeJson $
            [ "jsonCmd" /\ -- `jsonCmd` is the hack to make Argonaut properly encode the record
                { eventUID : E.uniqueId lbCommandEvt
                , command : cmd
                , keys : map Key.toString keys
                }
            ]
        ]
        [ ListBar.Command cmd keys /\ handler
        ]


select
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Int -> NodeKey subj id -> BlessedOp state m
select offset nodeKey =
    C.method nodeKey "select"
        [ C.arg CA.int offset
        ]


remove
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => String -> NodeKey subj id -> BlessedOp state m
remove item nodeKey =
    C.method nodeKey "remove"
        [ C.arg CA.string item
        ]


move
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Int -> NodeKey subj id -> BlessedOp state m
move offset nodeKey =
    C.method nodeKey "move"
        [ C.arg CA.int offset
        ]


moveLeft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Int -> NodeKey subj id -> BlessedOp state m
moveLeft offset nodeKey =
    C.method nodeKey "moveLeft"
        [ C.arg CA.int offset
        ]


moveRight
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Int -> NodeKey subj id -> BlessedOp state m
moveRight offset nodeKey =
    C.method nodeKey "moveRight"
        [ C.arg CA.int offset
        ]


selectTab
    :: forall (subj :: Subject) (id :: Symbol) state m
     . E.Fires subj ListBar.ListBarEvent
    => Respresents ListBar subj id
    => Int -> NodeKey subj id -> BlessedOp state m
selectTab index nodeKey =
    C.method nodeKey "selectTab"
        [ C.arg CA.int index
        ]


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