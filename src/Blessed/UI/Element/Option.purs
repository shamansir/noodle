module Blessed.UI.Element.Option where

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
import Blessed.Core.Color (Color)
import Blessed.Core.Color as Color
import Blessed.Core.Padding (Padding)
import Blessed.Core.Padding as Padding
import Blessed.Core.Flex (Flex)
import Blessed.Core.Align (HAlign, VAlign)
import Blessed.Core.Style (StyleOption)
import Blessed.Core.Border (Border, BorderOption)

import Blessed.Internal.Core (Attribute, option) as C

import Blessed.UI.Box.Event (Event)


type OptionsRow r =
    ( fg :: Color
    , bg :: Color
    , bold :: Boolean
    , underline :: Boolean
    , style :: Array (StyleOption ())
    , border :: Array (BorderOption ())
    , content :: String -- a ?
    , clickable :: Boolean
    , input :: Boolean
    , keyable :: Boolean
    , focused :: Boolean
    , hidden :: Boolean
    , label :: String
    , hoverText :: String
    , align :: HAlign
    , valign :: VAlign
    , shrink :: Flex
    , padding :: Padding
    , width :: Dimension
    , height :: Dimension
    , left :: Offset
    , right :: Offset
    , top :: Offset
    , bottom :: Offset
    -- , position ::
    , scrollable :: Boolean
    , ch :: Char
    , draggable :: Boolean
    , shadow :: Boolean


    , tags :: Boolean
    , hover :: Array (StyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ElementAttribute r = C.Attribute (OptionsRow + r) Event


boxOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ElementAttribute r
boxOption = C.option


top ∷ forall r. Offset -> ElementAttribute ( top :: Offset | r )
top = boxOption ( Proxy :: Proxy "top" )


left ∷ forall r. Offset -> ElementAttribute ( left :: Offset | r )
left = boxOption ( Proxy :: Proxy "left" )


width ∷ forall r. Dimension -> ElementAttribute ( width :: Dimension | r )
width = boxOption ( Proxy :: Proxy "width" )


height ∷ forall r. Dimension -> ElementAttribute ( height :: Dimension | r )
height = boxOption ( Proxy :: Proxy "height" )


content ∷ forall r. String -> ElementAttribute ( content :: String | r )
content = boxOption ( Proxy :: Proxy "content" )


tags ∷ forall r. Boolean -> ElementAttribute ( tags :: Boolean | r )
tags = boxOption ( Proxy :: Proxy "tags" )


draggable ∷ forall r. Boolean -> ElementAttribute ( draggable :: Boolean | r )
draggable = boxOption ( Proxy :: Proxy "draggable" )


-- style ∷ forall r sr. Array (StyleOption sr) -> ElementAttribute ( style :: Array (StyleOption sr) | r )
-- style = unsafeCoerce <<< boxOption ( Proxy :: Proxy "style" )

style ∷ forall r. Array (StyleOption ()) -> ElementAttribute ( style :: Array (StyleOption ()) | r )
style = boxOption ( Proxy :: Proxy "style" )


border ∷ forall r. Array (BorderOption ()) -> ElementAttribute ( border :: Array (BorderOption ()) | r )
border = boxOption ( Proxy :: Proxy "border" )