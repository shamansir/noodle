module Blessed.UI.Lists.List.Method where

import Prelude

import Blessed.Internal.NodeKey (RawNodeKey)


data Item
    = AtIndex Int
    | Element RawNodeKey
    -- TODO: Node

-- Item : ElementId | Element | Index | String

{- TODO -}

-- Child can be an element, index, or string.


{- FROM Chat GPT -}

{-

addItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => String -> NodeKey subj id -> BlessedOp state m
addItem text nodeId =
    C.method nodeId "addItem"
        [ C.arg CA.string text
        ]

removeItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Item -> NodeKey subj id -> BlessedOp state m
removeItem child nodeId =
    C.method nodeId "removeItem"
        [ C.arg CA.element child
        ]

pushItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Item -> NodeKey subj id -> BlessedOp state m
pushItem child nodeId =
    C.method nodeId "pushItem"
        [ C.arg CA.element child
        ]

popItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => NodeKey subj id -> BlessedOp state m (Element)
popItem nodeId =
    C.method nodeId "popItem" []

unshiftItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Item -> NodeKey subj id -> BlessedOp state m
unshiftItem child nodeId =
    C.method nodeId "unshiftItem"
        [ C.arg CA.element child
        ]

shiftItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => NodeKey subj id -> BlessedOp state m (Element)
shiftItem nodeId =
    C.method nodeId "shiftItem" []

insertItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Int -> Item -> NodeKey subj id -> BlessedOp state m
insertItem index child nodeId =
    C.method nodeId "insertItem"
        [ C.arg CA.int index
        , C.arg CA.element child
        ]

getItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Item -> NodeKey subj id -> BlessedOp state m (Element)
getItem child nodeId =
    C.method nodeId "getItem"
        [ C.arg CA.element child
        ]

setItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Item -> String -> NodeKey subj id -> BlessedOp state m
setItem child content nodeId =
    C.method nodeId "setItem"
        [ C.arg CA.element child
        , C.arg CA.string content
        ]


spliceItem
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Int -> Int -> ArrayItem -> NodeKey subj id -> BlessedOp state m
spliceItem i n items nodeId =
    C.method nodeId "spliceItem"
        [ C.arg CA.int i
        , C.arg CA.int n
        , C.arg CA.array items
        ]

clearItems
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => NodeKey subj id -> BlessedOp state m
clearItems nodeId =
    C.method nodeId "clearItems" []

setItems
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => ArrayItem -> NodeKey subj id -> BlessedOp state m
setItems items nodeId =
    C.method nodeId "setItems"
        [ C.arg CA.array items
        ]

getItemIndex
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Child -> NodeKey subj id -> BlessedOp state m (Int)
getItemIndex child nodeId =
    C.method nodeId "getItemIndex"
        [ C.arg CA.element child
        ]

select
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Int -> NodeKey subj id -> BlessedOp state m
select index nodeId =
    C.method nodeId "select"
        [ C.arg CA.int index
        ]

move
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Int -> NodeKey subj id -> BlessedOp state m
move offset nodeId =
    C.method nodeId "move"
        [ C.arg CA.int offset
        ]

up
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Int -> NodeKey subj id -> BlessedOp state m
up amount nodeId =
    C.method nodeId "up"
        [ C.arg CA.int amount
        ]

down
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Int -> NodeKey subj id -> BlessedOp state m
down amount nodeId =
    C.method nodeId "down"
        [ C.arg CA.int amount
        ]

pick
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Blessed -> NodeKey subj id -> BlessedOp state m
pick callback nodeId =
    C.method nodeId "pick"
        [ C.arg CA.function callback
        ]


fuzzyFind
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => Context -> NodeKey subj id -> BlessedOp state m
fuzzyFind context nodeId =
    C.method nodeId "fuzzyFind"
        [ C.arg CA.context context
        ]

-}


{-
addItem text:String
removeItem child:Item
pushItem child:Item
popItem -> Item
unshiftItem child:Item
shiftItem -> Item
insertItem index:Int child:Item
getItem child:Item -> Element
setItem child:Item content:String
spliceItem i:Int n:Int items:ArrayItem
clearItems
setItems items:ArrayItem
getItemIndex child:Child -> Int
select index:Int
move offset:Int
up amount:Int
down amount:Int
pick callback:Blessed
fuzzyFind context
-}