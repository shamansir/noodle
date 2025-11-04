module Front.Shared.Keyboard where

import Prelude

import Affjax.Node (delete)
import CSS (space)
import Control.Alt (alt)
import Data.Array as Array
import Data.Newtype (class Newtype, ala, unwrap, wrap)
import Data.Options (opt)
import Web.TouchEvent.TouchEvent (ctrlKey)


newtype Combo =
    Combo
        { mods :: Array String
        , key :: String
        , special :: Boolean
        }

derive instance Newtype Combo _


key :: String -> Combo
key k = Combo { mods: [], key: k, special: false }


special :: Combo -> Combo
special = unwrap >>> _ { special = true } >>> wrap


mod :: String -> Combo -> Combo
mod m = unwrap >>> (\c -> c { mods = Array.snoc c.mods m }) >>> wrap


w_shift :: Combo -> Combo
w_shift = mod "shift"


w_cmd :: Combo -> Combo
w_cmd = mod "cmd"


w_ctrl :: Combo -> Combo
w_ctrl = mod "ctrl"


w_alt :: Combo -> Combo
w_alt = mod "alt"


w_opt :: Combo -> Combo
w_opt = mod "opt"


backspace :: Combo
backspace = special $ key "backspace"


escape :: Combo
escape = special $ key "escape"


enter :: Combo
enter = special $ key "enter"


space :: Combo
space = special $ key "space"


tab :: Combo
tab = special $ key "tab"


shift :: Combo
shift = special $ key "shift"


delete :: Combo
delete = special $ key "delete"


ctrl :: Combo
ctrl = special $ key "ctrl"


cmd :: Combo
cmd = special $ key "cmd"


alt :: Combo
alt = special $ key "alt"


opt :: Combo
opt = special $ key "opt"


plus :: Combo
plus = key "+"


minus :: Combo
minus = key "-"


arrows :: Combo
arrows = special $ key "arrow keys"