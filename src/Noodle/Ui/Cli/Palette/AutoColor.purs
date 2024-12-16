module Noodle.Ui.Cli.Palette.AutoColor where

import Prelude

import Noodle.Ui.Cli.Palette.Item (Item) as Palette
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Set.Hydra (step) as Hydra
import Noodle.Ui.Cli.Palette.Set.Catpuccin (catpuccin) as Catpuccin


group :: Array Palette.Item
group =
    [ Pico8.trueBlue
    , Pico8.orange
    , Pico8.darkRed
    , Pico8.darkerBlue
    , Hydra.step 0 "step-0"
    , Hydra.step 1 "step-1"
    , Hydra.step 2 "step-2"
    , Hydra.step 3 "step-3"
    , Hydra.step 4 "step-4"
    , Hydra.step 5 "step-5"
    , Hydra.step 6 "step-6"
    , Hydra.step 7 "step-7"
    , Hydra.step 8 "step-8"
    , Catpuccin.catpuccin.latte.rosewater
    , Catpuccin.catpuccin.latte.flamingo
    , Catpuccin.catpuccin.latte.pink
    , Catpuccin.catpuccin.latte.mauve
    , Catpuccin.catpuccin.latte.red
    , Catpuccin.catpuccin.latte.maroon
    , Catpuccin.catpuccin.latte.peach
    , Catpuccin.catpuccin.latte.yellow
    , Catpuccin.catpuccin.latte.green
    , Catpuccin.catpuccin.latte.teal
    , Catpuccin.catpuccin.latte.sky
    , Catpuccin.catpuccin.latte.sapphire
    , Catpuccin.catpuccin.latte.blue
    , Catpuccin.catpuccin.latte.lavender
    , Catpuccin.catpuccin.frappe.rosewater
    , Catpuccin.catpuccin.frappe.flamingo
    , Catpuccin.catpuccin.frappe.pink
    , Catpuccin.catpuccin.frappe.mauve
    , Catpuccin.catpuccin.frappe.red
    , Catpuccin.catpuccin.frappe.maroon
    , Catpuccin.catpuccin.frappe.peach
    , Catpuccin.catpuccin.frappe.yellow
    , Catpuccin.catpuccin.frappe.green
    , Catpuccin.catpuccin.frappe.teal
    , Catpuccin.catpuccin.frappe.sky
    , Catpuccin.catpuccin.frappe.sapphire
    , Catpuccin.catpuccin.frappe.blue
    , Catpuccin.catpuccin.frappe.lavender
    ]