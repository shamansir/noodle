module Cli.Style where

import Prelude (($))

import Cli.Palette as Palette


import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Boxes.Box.Option as Box

import Blessed.Internal.NodeKey (class Respresents)
import Blessed.Internal.BlessedSubj (Element, Box, List, Line, class Extends)

import Blessed.UI.Boxes.Line.Option (ch, fg, bg, orientation, type_) as Line

import Blessed.Internal.Core (Attribute) as C
import Blessed.Core.Border as Border
import Blessed.Core.Orientation as Orientation
import Blessed.Core.Style as Style
import Blessed.Core.EndStyle as ES
import Blessed.Core.ListStyle as LStyle


library :: forall subj id state e r. Respresents List subj id => List.StyleAttrubute subj id state e r
library =
    List.style
        [ LStyle.bg Palette.libraryBg'
        , LStyle.item [ ES.fg Palette.libraryFg' ]
        , LStyle.selected
            [ ES.bg Palette.libraryBg'
            , ES.fg Palette.librarySelection'
            ]
        , LStyle.border
            [ Border.fg Palette.libraryBorder'
            , Border.bg Palette.libraryBg'
            ]
        ]


libraryBorder :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.BorderAttrubute subj id state e r
libraryBorder =
    Box.border
        [ Border.type_ Border._line
        , Border.fg Palette.libraryFg'
        ]


patchBox :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.StyleAttrubute subj id state e r
patchBox =
    Box.style
        [ Style.fg Palette.fg'
        , Style.bg Palette.patchBg'
        , Style.border
            [ Border.fg Palette.border'
            , Border.bg Palette.patchBg'
            ]
        ]


patchBoxBorder :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.BorderAttrubute subj id state e r
patchBoxBorder =
    Box.border
        [ Border.type_ Border._line
        ]


patchesBar :: forall subj id state e r. Respresents List subj id => List.StyleAttrubute subj id state e r
patchesBar =
    List.style
        [ LStyle.bg Palette.networkBg'
        , LStyle.item
            [ ES.fg Palette.itemNotSelected'
            , ES.bg Palette.networkBg'
            ]
        , LStyle.selected
            [ ES.fg Palette.itemSelected'
            , ES.bg Palette.networkBg'
            ]
        ]


addPatch :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.StyleAttrubute subj id state e r
addPatch =
    Box.style
        [ Style.fg Palette.fg'
        , Style.bg Palette.networkBg'
        ]


nodeBox :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.StyleAttrubute subj id state e r
nodeBox =
    Box.style
        [ Style.fg Palette.nodeFg'
        , Style.bg Palette.nodeBg'
        , Style.border
            [ Border.fg Palette.nodeBorder'
            , Border.bg Palette.nodeBg'
            ]
        , Style.focus
            [ ES.border
                [ Border.bg Palette.nodeBg'
                , Border.fg Palette.nodeSelection'
                ]
            ]
        ]


nodeBoxBorder :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.BorderAttrubute subj id state e r
nodeBoxBorder =
    Box.border
        [ Border.type_ Border._line
        , Border.fg $ Palette.nodeBorder'
        , Border.ch $ Border.fill ':'
        ]


inletsOutlets :: forall subj id state e r. Respresents List subj id => List.StyleAttrubute subj id state e r
inletsOutlets =
    List.style
        [ LStyle.bg Palette.libraryBg'
        , LStyle.item
            [ ES.fg Palette.itemNotSelected'
            , ES.bg Palette.libraryBg'
            ]
        , LStyle.selected
            [ ES.fg Palette.itemSelected'
            , ES.bg Palette.libraryBg'
            ]
        ]


linkA :: forall subj id state e. Respresents Line subj id => Array (C.Attribute subj id _ state e)
linkA =
    [ Line.orientation $ Orientation.Vertical
    , Line.ch '≀'
    , Line.fg Palette.linkFg'
    , Line.bg Palette.patchBg'
    ]


linkB :: forall subj id state e. Respresents Line subj id => Array (C.Attribute subj id _ state e)
linkB =
    [ Line.orientation $ Orientation.Horizontal
    , Line.type_ $ Border._bg
    , Line.ch '∼'
    , Line.fg Palette.linkFg'
    , Line.bg Palette.patchBg'
    ]


linkC :: forall subj id state e. Respresents Line subj id => Array (C.Attribute subj id _ state e)
linkC =
    [ Line.orientation $ Orientation.Vertical
    , Line.type_ $ Border._bg
    , Line.ch '≀'
    , Line.fg Palette.linkFg'
    , Line.bg Palette.patchBg'
    ]