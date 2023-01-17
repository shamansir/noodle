module Blessed.UI.Box.Option where

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
import Blessed.Core.Style (StyleOption)
import Blessed.Core.Border (Border, BorderOption)

import Blessed.Internal.Core (Attribute, option) as C

import Blessed.UI.Box.Event (Event)


type OptionsRow r =
    ( top :: Offset
    , left :: Offset
    , width :: Dimension
    , height :: Dimension
    , content :: String -- a ?
    , tags :: Boolean
    , draggable :: Boolean
    , hover :: (forall sr. Array (StyleOption sr))
    , style :: (forall sr. Array (StyleOption sr))
    , border :: (forall br. Array (BorderOption br))
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
    , hover : ([] :: forall sr. Array (StyleOption sr))
    , style : ([] :: forall sr. Array (StyleOption sr))
    , border : ([] :: forall br. Array (BorderOption br))
    }


type BoxAttribute r = C.Attribute (OptionsRow + r) Event


boxOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> BoxAttribute r
boxOption = C.option


top ∷ forall r. Offset -> BoxAttribute ( top :: Offset | r )
top = boxOption ( Proxy :: Proxy "top" )


left ∷ forall r. Offset -> BoxAttribute ( left :: Offset | r )
left = boxOption ( Proxy :: Proxy "left" )


width ∷ forall r. Dimension -> BoxAttribute ( width :: Dimension | r )
width = boxOption ( Proxy :: Proxy "width" )


height ∷ forall r. Dimension -> BoxAttribute ( height :: Dimension | r )
height = boxOption ( Proxy :: Proxy "height" )


content ∷ forall r. String -> BoxAttribute ( content :: String | r )
content = boxOption ( Proxy :: Proxy "content" )


tags ∷ forall r. Boolean -> BoxAttribute ( tags :: Boolean | r )
tags = boxOption ( Proxy :: Proxy "tags" )


draggable ∷ forall r. Boolean -> BoxAttribute ( draggable :: Boolean | r )
draggable = boxOption ( Proxy :: Proxy "draggable" )


style ∷ forall sr r. Array (StyleOption sr) -> BoxAttribute ( style :: Array (StyleOption sr) | r )
style = unsafeCoerce <<< boxOption ( Proxy :: Proxy "style" )


border ∷ forall r. Border -> BoxAttribute ( border :: Border | r )
border = boxOption ( Proxy :: Proxy "border" )