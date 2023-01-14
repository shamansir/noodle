module Blessed.UI.Box where


import Prelude (identity, Unit, ($))

import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))


import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dim
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Style (Style)
import Blessed.Core.Style as Style
import Blessed.Core.Border (Border)
import Blessed.Core.Border as Border


import Blessed.Internal.Command (Command, NodeId, call, arg) as C
import Blessed.Internal.Core (Prop, prop, Node, NodeAnd, node, nodeAnd, class Events, CoreEvent(..)) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (perform) as Op


data Event
    = Init
    | Key Unit
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
    , hover :: (Style -> Style)
    , style :: Style
    , border :: Border
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
    , hover : identity
    , style : Style.default
    , border : Border.default
    }


define ∷ forall (r ∷ Row Type)
    . Union r (OptionsRow ()) (OptionsRow ())
    ⇒ Nub r ((OptionsRow ()))
    ⇒ Record r → Options
define rec =
    Record.merge rec default


boxProp :: forall a r r' sym m. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> C.Prop (OptionsRow + r) m Event
boxProp = C.prop


top ∷ forall r m. Offset -> C.Prop ( top :: Offset | OptionsRow + r ) m Event
top = boxProp ( Proxy :: Proxy "top" )


left ∷ forall r m. Offset -> C.Prop ( left :: Offset | OptionsRow + r ) m Event
left = boxProp ( Proxy :: Proxy "left" )


width ∷ forall r m. Dimension -> C.Prop ( width :: Dimension | OptionsRow + r ) m Event
width = boxProp ( Proxy :: Proxy "width" )


height ∷ forall r m. Dimension -> C.Prop ( height :: Dimension | OptionsRow + r ) m Event
height = boxProp ( Proxy :: Proxy "height" )


content ∷ forall r m. String -> C.Prop ( content :: String | OptionsRow + r ) m Event
content = boxProp ( Proxy :: Proxy "content" )


tags ∷ forall r m. Boolean -> C.Prop ( tags :: Boolean | OptionsRow + r ) m Event
tags = boxProp ( Proxy :: Proxy "tags" )


draggable ∷ forall r m. Boolean -> C.Prop ( draggable :: Boolean | OptionsRow + r ) m Event
draggable = boxProp ( Proxy :: Proxy "draggable" )


style ∷ forall r m. Style -> C.Prop ( style :: Style | OptionsRow + r ) m Event
style = boxProp ( Proxy :: Proxy "style" )


border ∷ forall r m. Border -> C.Prop ( border :: Border | OptionsRow + r ) m Event
border = boxProp ( Proxy :: Proxy "border" )


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