module Blessed.UI.Screen where

import Prelude


import Blessed.UI.Box as Box

import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record


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



{- define' :: forall r. SubRecord r -> Options
define' rec =
    withDefaults default rec
    -- define { title : "foo" } -}