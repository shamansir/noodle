module Noodle.Ui.Cli.Palette.AutoColor where

import Prelude

import Noodle.Ui.Cli.Palette.Item (Item) as Palette
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Set.Hydra (steps) as Hydra
import Noodle.Ui.Cli.Palette.Set.Catpuccin (collectColors, latte, frappe, mocha, macchiato) as Catpuccin


group :: Array Palette.Item
group =
    [ Pico8.trueBlue
    , Pico8.orange
    , Pico8.darkRed
    , Pico8.darkerBlue
    ]
    <> (Hydra.steps 8 $ \n -> show n <> "-step")
    <> Catpuccin.collectColors Catpuccin.latte
    <> Catpuccin.collectColors Catpuccin.frappe
    <> Catpuccin.collectColors Catpuccin.mocha
    <> Catpuccin.collectColors Catpuccin.macchiato