module Blessed.UI.Screen.Property where


import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Internal.Core (Attribute, property) as C
import Blessed.UI.Screen.Event (Event)
import Blessed.UI.Box.Property as Box


type PropertiesRow :: Row Type -> Row Type
type PropertiesRow r =
    ( title :: String
    , smartCSR :: Boolean
    | Box.PropertiesRow + r
    )
type PropertiesU = PropertiesRow ()
type Properties = Record (PropertiesU)


{- default :: Options
default =
    Record.merge
        { title : ""
        , smartCSR : false
        }
        Box.default -}


{- define ∷ forall (r ∷ Row Type)
    . Union r (PropertiesRow ()) (PropertiesRow ())
    ⇒ Nub r ((PropertiesRow ()))
    ⇒ Record r → Options
define rec =
    Record.merge rec default -}



type ScreenAttribute r = C.Attribute (PropertiesRow + r) Event


screenProperty :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> ScreenAttribute r
screenProperty = C.property



--draggable :: forall r e. Boolean -> Prop ( draggable :: Boolean | r ) e

title ∷ forall r. String -> ScreenAttribute ( title :: String | r )
title = screenProperty ( Proxy :: Proxy "title" )


smartCSR ∷ forall r. Boolean -> ScreenAttribute ( smartCSR :: Boolean | r )
smartCSR = screenProperty ( Proxy :: Proxy "smartCSR" )
