module Noodle.Ui.Cli.Palette.AutoColor where

import Prelude

import Noodle.Ui.Cli.Palette.Item (Item) as Palette
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Set.Hydra as Hydra
import Noodle.Ui.Cli.Palette.Set.Catpuccin as Catpuccin


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
    , Catpuccin.catpuccinP.latte.rosewater
    , Catpuccin.catpuccinP.latte.flamingo
    , Catpuccin.catpuccinP.latte.pink
    , Catpuccin.catpuccinP.latte.mauve
    , Catpuccin.catpuccinP.latte.red
    , Catpuccin.catpuccinP.latte.maroon
    , Catpuccin.catpuccinP.latte.peach
    , Catpuccin.catpuccinP.latte.yellow
    , Catpuccin.catpuccinP.latte.green
    , Catpuccin.catpuccinP.latte.teal
    , Catpuccin.catpuccinP.latte.sky
    , Catpuccin.catpuccinP.latte.sapphire
    , Catpuccin.catpuccinP.latte.blue
    , Catpuccin.catpuccinP.latte.lavender
    , Catpuccin.catpuccinP.frappe.rosewater
    , Catpuccin.catpuccinP.frappe.flamingo
    , Catpuccin.catpuccinP.frappe.pink
    , Catpuccin.catpuccinP.frappe.mauve
    , Catpuccin.catpuccinP.frappe.red
    , Catpuccin.catpuccinP.frappe.maroon
    , Catpuccin.catpuccinP.frappe.peach
    , Catpuccin.catpuccinP.frappe.yellow
    , Catpuccin.catpuccinP.frappe.green
    , Catpuccin.catpuccinP.frappe.teal
    , Catpuccin.catpuccinP.frappe.sky
    , Catpuccin.catpuccinP.frappe.sapphire
    , Catpuccin.catpuccinP.frappe.blue
    , Catpuccin.catpuccinP.frappe.lavender
    ]