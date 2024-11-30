module Cli.Components.PaletteTest where

import Prelude

import Data.Text.Format (Tag)
import Data.Text.Format as T
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Data.Text.Output.Blessed (multiLine) as Blessed

import Blessed as B
import Blessed ((>~))
import Blessed.Core.Key (alpha, control, escape) as Key
import Blessed.Core.Coord ((<+>))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.Core as Core
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.Internal.BlessedSubj (Box, Screen)
import Blessed.Internal.NodeKey (nk, type (<^>))
import Blessed.UI.Base.Screen.Event (key) as Screen
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Screen.Option (fullUnicode, smartCSR, title) as Screen

import Noodle.Id as Id
import Noodle.Ui.Cli.Tagging as T
import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Palette.Set.Hydra as Hydra
import Noodle.Ui.Cli.Palette.Item as Palette

import Starter.Toolkit (STARTER)
import StarterTk.Simple.Sum as Starter.Sum
import StarterTk.Simple.Color as Starter.Color
import Demo.Toolkit.Starter.Repr (StarterRepr(..))


type MainScreenKey = Screen <^> "main"
type PaletteTestKey = Box <^> "palette-test"


mainScreenKey  = nk :: MainScreenKey
paletteTestKey = nk :: PaletteTestKey


paletteBox
    :: Core.Blessed Unit
paletteBox =
    B.boxAnd paletteTestKey

        [ Box.top    $ Offset.calc $ Coord.center <+> Coord.px 1
        , Box.left   $ Offset.center
        , Box.width  $ Dimension.percents 100.0
        , Box.height $ Dimension.percents 100.0
        , Box.content $ Blessed.multiLine $ foldl (<>) T.nil buildPalette
        , Box.tags true
        ]

        [
        ]

        $ \_ -> do pure unit


buildPalette :: Array Tag
buildPalette =
    [ T.s "Pico 8 / FG :: [[" ] <>
    (T.paletteFg <$> Pico8.pico8) <>
    [ T.s "]] Pico 8 / BG :: [[ " ] <>
    (T.paletteBg <$> Pico8.pico8) <>
    [ T.s "]] Hydra fn-s / FG :: [[" ] <>
    (T.paletteFg <$> Hydra.hydraFns) <>
    [ T.s "]] Hydra fn-s / BG :: [[ " ] <>
    (T.paletteBg <$> Hydra.hydraFns) <>
    [ T.s "]] X11 / FG :: [[" ] <>
    (T.paletteFg <$> X11.x11colors) <>
    [ T.s "]] X11 / BG :: [[ " ] <>
    (T.paletteBg <$> X11.x11colors) <>
    [ T.s "]]"
    , T.wraps "empty-i-0<" ">" $ T.inlet 0 Starter.Sum._in_a (Nothing :: Maybe StarterRepr)
    , T.wraps "empty-i-1<" ">" $ T.inlet 1 Starter.Sum._in_b (Nothing :: Maybe StarterRepr)
    , T.wraps "num-i-0<" ">" $ T.inlet 0 Starter.Sum._in_a $ Just $ VNumber 10.0
    , T.wraps "num-i-1<" ">" $ T.inlet 1 Starter.Sum._in_b $ Just $ VNumber 20.0
    , T.wraps "char-i-1<" ">" $ T.inlet 1 Starter.Sum._in_b $ Just $ VChar 'a'
    , T.wraps "i-infobox<" ">" $ T.inletInfoBox Starter.Sum._in_b
    , T.wraps "empty-i-sl<" ">" $ T.inletStatusLine Starter.Sum._sum 1 Starter.Sum._in_b (Nothing :: Maybe StarterRepr)
    , T.wraps "num-i-sl<" ">" $ T.inletStatusLine Starter.Sum._sum 1 Starter.Sum._in_b $ Just $ VNumber 20.0
    , T.wraps "empty-o-0<" ">" $ T.outlet 0 Starter.Sum._out_sum (Nothing :: Maybe StarterRepr)
    , T.wraps "empty-o-1<" ">" $ T.outlet 0 Starter.Color._out_color (Nothing :: Maybe StarterRepr)
    , T.wraps "num-o-0<" ">" $ T.outlet 0 Starter.Sum._out_sum $ Just $ VNumber 10.0
    , T.wraps "char-o-1<" ">" $ T.outlet 1 Starter.Sum._out_sum $ Just $ VChar 'a'
    , T.wraps "o-infobox<" ">" $ T.outletInfoBox Starter.Sum._out_sum
    , T.wraps "empty-o-sl<" ">" $ T.outletStatusLine Starter.Sum._sum 1 Starter.Sum._out_sum (Nothing :: Maybe StarterRepr)
    , T.wraps "num-o-sl<" ">" $ T.outletStatusLine Starter.Sum._sum 1 Starter.Sum._out_sum $ Just $ VNumber 20.0
    -- TODO , T.wraps "node-lbl-1<" ">" $ T.nodeLabel Starter.Sum._sum
    -- TODO , T.wraps "node-lbl-2<" ">" $ T.nodeLabel Starter.Color._color
    , T.wraps "rembtn-out<" ">" $ T.removeButtonOut
    , T.wraps "rembtn-over<" ">" $ T.removeButtonOver
    , T.wraps "rembtn-infobox<" ">" $ T.removeInfoBox
    , T.wraps "rem-sl-1<" ">" $ T.removeStatusLine Starter.Sum._sum
    , T.wraps "rem-sl-2<" ">" $ T.removeStatusLine Starter.Color._color
    , T.wraps "libitem-1<" ">" $ T.libraryItem (Proxy :: _ STARTER) $ Id.familyR Starter.Color._color
    , T.wraps "libitem-2<" ">" $ T.libraryItem (Proxy :: _ STARTER) $ Id.familyR Starter.Sum._sum
    , T.wraps "cmd-tk<" ">" $ T.toolkit "Test"
    , T.wraps "cmd-tk-ver<" ">" $ T.tkVersion 2.1
    , T.wraps "cmd-ndf-ver<" ">" $ T.ndfVersion 2.1
    , T.wraps "cmd-family<" ">" $ T.family "family"
    , T.wraps "cmd-some-grp<" ">" $ T.someGroup "group"
    ]


component
    :: Core.Blessed Unit
component =
    B.screenAnd mainScreenKey

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> B.exit
        ]

        [ paletteBox
        ]

        $ \_ -> do
            mainScreenKey >~ Screen.render
