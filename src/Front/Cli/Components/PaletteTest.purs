module Cli.Components.PaletteTest where

import Prelude

import Data.Text.Format (Tag)
import Data.Text.Format as T
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
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
import Noodle.Ui.Cli.Tagging.At as At
import Noodle.Ui.Cli.Tagging.At (class At)
import Noodle.Ui.Cli.Tagging.At (ChannelLabel, channelLabel) as At
import Noodle.Ui.Cli.Palette as P
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Palette.Set.Hydra as Hydra
import Noodle.Ui.Cli.Palette.Set.Catpuccin as Catpuccin
import Noodle.Ui.Cli.Palette.Item as Palette
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (accept, decline) as ViC
import Noodle.Fn.Shape.Temperament (Temperament(..))

import Starter.Toolkit (STARTER)
import StarterTk.Simple.Sum as Starter.Sum
import StarterTk.Simple.Color as Starter.Color
import Demo.Toolkit.Starter.Repr.ChRepr (ValueRepr)
import Demo.Toolkit.Starter.Repr.ChRepr (ValueRepr(..), Time(..), Shape(..), Color(..), Spread(..)) as Sr


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
    [ T.s "]] CPCN / FG :: [[" ] <>
    (T.paletteFg <$> Catpuccin.catpuccinAll) <>
    [ T.s "]] CPCN / BG :: [[ " ] <>
    (T.paletteBg <$> Catpuccin.catpuccinAll) <>
    [ T.s "]] X11 / FG :: [[" ] <>
    (T.paletteFg <$> X11.x11colors) <>
    [ T.s "]] X11 / BG :: [[ " ] <>
    (T.paletteBg <$> X11.x11colors) <>
    [ T.s "]]"
    , qt "empty-i-0" $ T.inlet 0 (Id.inletR Starter.Sum._in_a) (ViC.decline :: ValueInChannel ValueRepr)
    , qt "empty-i-1" $ T.inlet 1 (Id.inletR Starter.Sum._in_b) (ViC.decline :: ValueInChannel ValueRepr)
    , qt "num-i-0" $ T.inlet 0 (Id.inletR Starter.Sum._in_a) $ ViC.accept $ Sr.VNumber 10.0
    , qt "num-i-1" $ T.inlet 1 (Id.inletR Starter.Sum._in_b) $ ViC.accept $ Sr.VNumber 20.0
    , qt "char-i-1" $ T.inlet 1 (Id.inletR Starter.Sum._in_b) $ ViC.accept $ Sr.VChar 'a'
    , qt "i-infobox" $ T.inletInfoBox (Id.inletR Starter.Sum._in_b)
    , qt "empty-i-sl" $ T.inletStatusLine (Id.familyR Starter.Sum._sum) 1 (Id.inletR Starter.Sum._in_b) (ViC.decline :: ValueInChannel ValueRepr)
    , qt "num-i-sl" $ T.inletStatusLine (Id.familyR Starter.Sum._sum) 1 (Id.inletR Starter.Sum._in_b) $ ViC.accept $ Sr.VNumber 20.0
    , qt "empty-o-0" $ T.outlet 0 (Id.outletR Starter.Sum._out_sum) (ViC.decline :: ValueInChannel ValueRepr)
    , qt "empty-o-1" $ T.outlet 0 (Id.outletR Starter.Color._out_color) (ViC.decline :: ValueInChannel ValueRepr)
    , qt "num-o-0" $ T.outlet 0 (Id.outletR Starter.Sum._out_sum) $ ViC.accept $ Sr.VNumber 10.0
    , qt "char-o-1" $ T.outlet 1 (Id.outletR Starter.Sum._out_sum) $ ViC.accept $ Sr.VChar 'a'
    , qt "o-infobox" $ T.outletInfoBox (Id.outletR Starter.Sum._out_sum)
    , qt "empty-o-sl" $ T.outletStatusLine (Id.familyR Starter.Sum._sum) 1 (Id.outletR Starter.Sum._out_sum) (ViC.decline :: ValueInChannel ValueRepr)
    , qt "num-o-sl" $ T.outletStatusLine (Id.familyR Starter.Sum._sum) 1 (Id.outletR Starter.Sum._out_sum) $ ViC.accept $ Sr.VNumber 20.0
    -- TODO , qt "node-lbl-1" $ T.nodeLabel (Id.familyR Starter.Sum._sum)
    -- TODO , qt "node-lbl-2" $ T.nodeLabel Starter.Color._color
    , qt "rembtn-out" $ T.removeButtonOut
    , qt "rembtn-over" $ T.removeButtonOver
    , qt "rembtn-infobox" $ T.removeInfoBox
    , qt "rem-sl-1" $ T.removeStatusLine (Id.familyR Starter.Sum._sum)
    , qt "rem-sl-2" $ T.removeStatusLine (Id.familyR Starter.Color._color)
    , qt "libitem-1" $ T.libraryItem (Proxy :: _ STARTER) $ Id.familyR Starter.Color._color
    , qt "libitem-2" $ T.libraryItem (Proxy :: _ STARTER) $ Id.familyR Starter.Sum._sum
    , qt "btn-tgl-on" $ T.buttonToggle "H" true
    , qt "btn-tgl-off" $ T.buttonToggle "H" false
    , qt "btn-con-left" $ T.buttonConnection $ Left "W"
    , qt "btn-con-r-0" $ T.buttonConnection $ Right 0
    , qt "btn-con-r-1" $ T.buttonConnection $ Right 1
    , qt "o-hvr" $ T.outletHover
    , qt "o-sel" $ T.outletSelect
    , qt "i-hvr-hot" $ T.inletHover Hot
    , qt "i-hvr-cld" $ T.inletHover Cold
    -- TODO , qt "i-sel" $ T.inletSelect
    , qt "inode-num" $ T.infoNode $ Sr.VNumber 20.0
    , qt "inode-chr" $ T.infoNode $ Sr.VChar 'b'
    -- TODO , qt "f-docs" $ T.family-docs Starter.Color._color
    , qt "sel" $ T.selected "SEL"
    , qt "ord-i" $ T.orderItem "oi"
    , qt "ord-spl" $ T.orderSplit "|" -- FIXME: removing this item breaks file path highlight to direct code
    , qt "fpath" $ T.filePath "file://"
    , qt "cmd-tk" $ T.toolkit "Test"
    , qt "cmd-tk-ver" $ T.tkVersion 2.1
    , qt "cmd-ndf-ver" $ T.ndfVersion 2.1
    , qt "cmd-family" $ T.family "family"
    , qt "cmd-some-grp" $ T.someGroup "group"
    , qt "cmd-nodeid" $ T.nodeId "node-id"
    , qt "cmd-op" $ T.operator "~>"
    , qt "cmd-comment" $ T.comment "comment"
    , qt "cmd-val" $ T.value "12"
    , qt "cmd-coord" $ T.coord 5
    , qt "cmd-i-idx" $ T.inletIdx 1
    , qt "cmd-o-idx" $ T.outletIdx 3
    , qt "cmd-i-id" $ T.inletId "foo"
    , qt "cmd-o-id" $ T.outletId "bar"
    , qt "cmd-type" $ T.type_ "String"
    , qbg "node-bg" $ P.nodeBg
    ] <>
    (qchrepr <$>
        [ Sr.VNone, Sr.VBang, Sr.VNumber 5.0, Sr.VBool false, Sr.VBool true, Sr.VChar 'x'
        , Sr.VTime $ Sr.Time { hours : 1, minutes : 10, seconds : 5 }, Sr.VTime $ Sr.Time { hours : 0, minutes : 0, seconds : 12 }
        , Sr.VShape Sr.Circle, Sr.VShape Sr.Rect, Sr.VShape Sr.Cross, Sr.VShape Sr.Diamond
        , Sr.VColor $ Sr.Color { r : 255, g : 0, b : 0, a : 255 }, Sr.VColor $ Sr.Color { r : 255, g : 255, b : 0, a : 255 }, Sr.VColor $ Sr.Color { r : 255, g : 0, b : 255, a : 255 }
        , Sr.VSpreadNum $ Sr.Spread [ ], Sr.VSpreadNum $ Sr.Spread [ 2.0, 5.0 ]
        ]
    )


qt :: String -> Tag -> Tag
qt n = T.wraps (n <> "<") ">"


qbg :: String -> Palette.Item -> Tag
qbg n c = qt n $ T.bgcs (Palette.colorOf c) n


qfg :: String -> Palette.Item -> Tag
qfg n c = qt n $ T.fgcs (Palette.colorOf c) n


qchrepr :: forall repr. At At.ChannelLabel repr => repr -> Tag
qchrepr repr =
    -- TODO : from `inletId`` :: -- T.fgc (C.colorOf Palette.inletId) <<< T.s
    At.channelLabel repr


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
