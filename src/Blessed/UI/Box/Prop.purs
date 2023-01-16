module Blessed.UI.Box.Prop where

import Prelude ((<<<))

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Unsafe.Coerce (unsafeCoerce)


import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dim
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Style (StyleProperty)
import Blessed.Core.Border (Border, BorderProperty)

import Blessed.Internal.Core (Attribute, property) as C

import Blessed.UI.Box.Event (Event)


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


boxProperty :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> BoxAttribute r
boxProperty = C.property


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