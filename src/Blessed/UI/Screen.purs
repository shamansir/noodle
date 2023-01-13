module Blessed.UI.Screen where

import Prelude


import Blessed.UI.Box as Box
import Blessed.Internal.Core (Prop, prop)

import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Data.Symbol (reflectSymbol, class IsSymbol)

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


screenProp :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> Prop (OptionsRow + r)
screenProp = prop


--type ScreenProp (sym :: Symbol) = forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> Prop ( sym :: a | OptionsRow + r )


-- title :: forall r e. String -> Prop ( title :: String | r ) e
-- title = screenProp "title"


--draggable :: forall r e. Boolean -> Prop ( draggable :: Boolean | r ) e

title ∷ forall r. String -> Prop ( title :: String | OptionsRow + r )
title = screenProp ( Proxy :: Proxy "title" )


tags ∷ forall r. Boolean -> Prop ( tags :: Boolean | OptionsRow + r )
tags = screenProp ( Proxy :: Proxy "tags" )


{- define' :: forall r. SubRecord r -> Options
define' rec =
    withDefaults default rec
    -- define { title : "foo" } -}