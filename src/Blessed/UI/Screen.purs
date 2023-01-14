module Blessed.UI.Screen where

import Prelude


import Blessed.UI.Box as Box
import Blessed.Internal.Core (Prop, prop, Node, node, class Events, CoreEvent(..)) as C

import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))


import Data.Symbol (reflectSymbol, class IsSymbol)


data Event
    = Init
    | Other


instance events :: C.Events Event where
    initial = Init
    convert Init = "init"
    convert Other = "?"
    toCore _ = C.CoreEvent
    fromCore _ = Nothing


type OptionsRow r =
    ( title :: String
    , smartCSR :: Boolean
    | Box.OptionsRow + r
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)


default :: Options
default =
    Record.merge
        { title : ""
        , smartCSR : false
        }
        Box.default


define ∷ forall (r ∷ Row Type)
    . Union r (OptionsRow ()) (OptionsRow ())
    ⇒ Nub r ((OptionsRow ()))
    ⇒ Record r → Options
define rec =
    Record.merge rec default


screenProp :: forall a r r' sym m. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> C.Prop (OptionsRow + r) m Event
screenProp = C.prop



--draggable :: forall r e. Boolean -> Prop ( draggable :: Boolean | r ) e

title ∷ forall r m. String -> C.Prop ( title :: String | OptionsRow + r ) m Event
title = screenProp ( Proxy :: Proxy "title" )


tags ∷ forall r m. Boolean -> C.Prop ( tags :: Boolean | OptionsRow + r ) m Event
tags = screenProp ( Proxy :: Proxy "tags" )


screen :: forall r m. String -> C.Node ( OptionsRow + r ) m Event
screen name = C.node name


screenAnd :: forall r m. String -> C.Node ( OptionsRow + r ) m Event
screenAnd name = C.node name