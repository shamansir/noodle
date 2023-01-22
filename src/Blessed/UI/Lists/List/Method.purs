module Blessed.UI.Lists.List.Method where

import Prelude

import Blessed.Internal.JsApi as I


data Item
    = AtIndex Int
    | Element I.NodeId
    -- TODO: Node

-- Item : ElementId | Element | Index | String


{-
addItem text:String
removeItem child:Item
pushItem child:Item
popItem -> Item
unshiftItem child:Item
shiftItem -> Item
inserItem index:Int child:Item
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