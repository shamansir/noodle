module Blessed.UI.Box where


import Prelude (identity, Unit, ($), (<<<))

import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)


import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dim
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Style (Style, StyleProperty)
import Blessed.Core.Style as Style
import Blessed.Core.Border (Border, BorderProperty)
import Blessed.Core.Border as Border
import Blessed.Core.Key (Key)


import Blessed.Internal.Command (Command, call, arg) as C
import Blessed.Internal.Core (Attribute, property, Node, NodeAnd, node, nodeAnd, class Events, CoreEvent(..), handler, Handler) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.JsApi (Kind(..)) as Kind
import Blessed.Internal.BlessedOp (perform) as Op


data Event
    = Init
    | Key (Array Key)
    | Click
    | Other


instance events :: C.Events Event where
    initial = Init
    convert Init = "init"
    convert (Key key) = "key"
    convert Click = "click"
    convert Other = "?"
    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type OptionsRow r =
    ( top :: Offset
    , left :: Offset
    , width :: Dimension
    , height :: Dimension
    , content :: String -- a ?
    , tags :: Boolean
    , draggable :: Boolean
    , hover :: (forall sr. Array (StyleProperty sr))
    , style :: (forall sr. Array (StyleProperty sr))
    , border :: (forall br. Array (BorderProperty br))
    | r
    )
type Options = Record (OptionsRow ())


default :: Options
default =
    { top : Offset.px 0
    , left : Offset.px 0
    , width : Dim.percents 100.0
    , height : Dim.percents 100.0
    , content : ""
    , tags : false
    , draggable : false
    , hover : ([] :: forall sr. Array (StyleProperty sr))
    , style : ([] :: forall sr. Array (StyleProperty sr))
    , border : ([] :: forall br. Array (BorderProperty br))
    }


type BoxAttribute r = C.Attribute (OptionsRow + r) Event
type BoxHandler r = C.Handler r Event


boxProperty :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> BoxAttribute r
boxProperty = C.property


boxHandler :: forall r. Event -> BoxHandler r
boxHandler = C.handler


top ∷ forall r. Offset -> BoxAttribute ( top :: Offset | r )
top = boxProperty ( Proxy :: Proxy "top" )


left ∷ forall r. Offset -> BoxAttribute ( left :: Offset | r )
left = boxProperty ( Proxy :: Proxy "left" )


width ∷ forall r. Dimension -> BoxAttribute ( width :: Dimension | r )
width = boxProperty ( Proxy :: Proxy "width" )


height ∷ forall r. Dimension -> BoxAttribute ( height :: Dimension | r )
height = boxProperty ( Proxy :: Proxy "height" )


content ∷ forall r. String -> BoxAttribute ( content :: String | r )
content = boxProperty ( Proxy :: Proxy "content" )


tags ∷ forall r. Boolean -> BoxAttribute ( tags :: Boolean | r )
tags = boxProperty ( Proxy :: Proxy "tags" )


draggable ∷ forall r. Boolean -> BoxAttribute ( draggable :: Boolean | r )
draggable = boxProperty ( Proxy :: Proxy "draggable" )


style ∷ forall sr r. Array (StyleProperty sr) -> BoxAttribute ( style :: Array (StyleProperty sr) | r )
style = unsafeCoerce <<< boxProperty ( Proxy :: Proxy "style" )


border ∷ forall r. Border -> BoxAttribute ( border :: Border | r )
border = boxProperty ( Proxy :: Proxy "border" )


key :: forall r. Array Key -> BoxHandler r
key = boxHandler <<< Key


on :: forall r. Event -> BoxHandler r
on = boxHandler


click :: Event
click = Click


box :: forall r. String -> C.Node ( OptionsRow + r ) Event
box name = C.node Kind.Box name


boxAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
boxAnd name = C.nodeAnd Kind.Box name



setContent :: forall m. C.NodeId -> String -> BlessedOp m
setContent nodeId value =
    Op.perform nodeId
        $ C.call "setContent"
            [ C.arg CA.string value
            ]


setLine :: forall m. C.NodeId -> Int -> String -> BlessedOp m
setLine nodeId n value =
    Op.perform nodeId
        $ C.call "setLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


insertLine :: forall m. C.NodeId -> Int -> String -> BlessedOp m
insertLine nodeId n value =
    Op.perform nodeId
        $ C.call "insertLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


focus :: forall m. C.NodeId -> BlessedOp m
focus nodeId =
    Op.perform nodeId
        $ C.call "focus" []