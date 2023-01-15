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
import Blessed.Core.Style (Style, StyleProp)
import Blessed.Core.Style as Style
import Blessed.Core.Border (Border, BorderProp)
import Blessed.Core.Border as Border
import Blessed.Core.Key (Key)


import Blessed.Internal.Command (Command, NodeId, call, arg) as C
import Blessed.Internal.Core (Prop, prop, Node, NodeAnd, node, nodeAnd, class Events, CoreEvent(..), handler) as C
import Blessed.Internal.BlessedOp (BlessedOp)
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
    , hover :: (forall sr. Array (StyleProp sr))
    , style :: (forall sr. Array (StyleProp sr))
    , border :: (forall br. Array (BorderProp br))
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
    , hover : ([] :: forall sr. Array (StyleProp sr))
    , style : ([] :: forall sr. Array (StyleProp sr))
    , border : ([] :: forall br. Array (BorderProp br))
    }


type BoxProp r m = C.Prop (OptionsRow + r) m Event


boxProp :: forall a r r' sym m. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> BoxProp r m
boxProp = C.prop


boxHandler :: forall m r. Event -> BlessedOp m -> BoxProp r m
boxHandler = C.handler


top ∷ forall r m. Offset -> BoxProp ( top :: Offset | r ) m
top = boxProp ( Proxy :: Proxy "top" )


left ∷ forall r m. Offset -> BoxProp ( left :: Offset | r ) m
left = boxProp ( Proxy :: Proxy "left" )


width ∷ forall r m. Dimension -> BoxProp ( width :: Dimension | r ) m
width = boxProp ( Proxy :: Proxy "width" )


height ∷ forall r m. Dimension -> BoxProp ( height :: Dimension | r ) m
height = boxProp ( Proxy :: Proxy "height" )


content ∷ forall r m. String -> BoxProp ( content :: String | r ) m
content = boxProp ( Proxy :: Proxy "content" )


tags ∷ forall r m. Boolean -> BoxProp ( tags :: Boolean | r ) m
tags = boxProp ( Proxy :: Proxy "tags" )


draggable ∷ forall r m. Boolean -> BoxProp ( draggable :: Boolean | r ) m
draggable = boxProp ( Proxy :: Proxy "draggable" )


style ∷ forall sr r m. Array (StyleProp sr) -> BoxProp ( style :: Array (StyleProp sr) | r ) m
style = unsafeCoerce <<< boxProp ( Proxy :: Proxy "style" )


border ∷ forall r m. Border -> BoxProp ( border :: Border | r ) m
border = boxProp ( Proxy :: Proxy "border" )


key :: forall r m. Array Key -> BlessedOp m -> BoxProp r m
key = boxHandler <<< Key


box :: forall r m. String -> C.Node ( OptionsRow + r ) m Event
box name = C.node name


boxAnd :: forall r m. String -> C.NodeAnd ( OptionsRow + r ) m Event
boxAnd name = C.nodeAnd name



setContent :: forall m. C.NodeId -> String -> BlessedOp m
setContent nodeId value =
    Op.perform nodeId
        $ C.call nodeId "setContent"
            [ C.arg CA.string value
            ]


setLine :: forall m. C.NodeId -> Int -> String -> BlessedOp m
setLine nodeId n value =
    Op.perform nodeId
        $ C.call nodeId "setLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]


insertLine :: forall m. C.NodeId -> Int -> String -> BlessedOp m
insertLine nodeId n value =
    Op.perform nodeId
        $ C.call nodeId "insertLine"
            [ C.arg CA.int n
            , C.arg CA.string value
            ]