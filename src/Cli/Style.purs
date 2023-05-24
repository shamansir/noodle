module Cli.Style where

import Prelude (($))
import Type.Row (type (+))

import Cli.Palette as Palette


import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Boxes.Box.Option as Box

import Blessed.Internal.NodeKey (class Respresents)
import Blessed.Internal.BlessedSubj (Element, Box, List, Line, class Extends)

import Blessed.UI.Boxes.Line.Option (ch, fg, orientation, type_, LineAttribute, OptionsRow) as Line
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Core.Border (type_, _line, fg, bg, ch, fill, _bg) as Border
import Blessed.Core.Orientation as Orientation
import Blessed.Core.Style as Style
import Blessed.Core.EndStyle as ES
import Blessed.Core.ListStyle as LStyle


inletsOutlets :: forall subj id state e r. Respresents List subj id => List.StyleAttrubute subj id state e r
inletsOutlets =
    List.style
        [ LStyle.bg Palette.networkBackground'
        , LStyle.item
            [ ES.fg Palette.itemNotSelected'
            , ES.bg Palette.networkBackground'
            ]
        , LStyle.selected
            [ ES.fg Palette.itemSelected'
            , ES.bg Palette.networkBackground'
            ]
        ]

library :: forall subj id state e r. Respresents List subj id => List.StyleAttrubute subj id state e r
library =
    List.style
        [ LStyle.item [ ES.fg Palette.nodeListFg' ]
        , LStyle.selected [ ES.fg Palette.nodeListSelFg' ]
        ]


libraryBorder :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.BorderAttrubute subj id state e r
libraryBorder =
    Box.border
        [ Border.type_ Border._line
        , Border.fg Palette.nodeListFg'
        ]


patchBox :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.StyleAttrubute subj id state e r
patchBox =
    Box.style
        [ Style.fg Palette.foreground'
        , Style.bg Palette.patchBackground'
        , Style.border
            [ Border.fg Palette.border'
            , Border.bg Palette.patchBackground'
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
        [ LStyle.bg Palette.networkBackground'
        , LStyle.item
            [ ES.fg Palette.itemNotSelected'
            , ES.bg Palette.networkBackground'
            ]
        , LStyle.selected
            [ ES.fg Palette.itemSelected'
            , ES.bg Palette.networkBackground'
            ]
        ]


addPatch :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.StyleAttrubute subj id state e r
addPatch =
    Box.style
        [ Style.fg Palette.foreground'
        , Style.bg Palette.networkBackground'
        ]


nodeBoxBorder :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.BorderAttrubute subj id state e r
nodeBoxBorder =
    Box.border
        [ Border.type_ Border._line
        , Border.fg $ Palette.nodeBoxBorder'
        , Border.ch $ Border.fill ':'
        ]

nodeBox :: forall subj id state e r. Extends Element subj => Respresents Box subj id => Box.StyleAttrubute subj id state e r
nodeBox =
    Box.style
        [ Style.focus
            [ ES.border
                [ Border.fg Palette.nodeListSelFg'
                ]
            ]
        ]


linkA :: forall subj id state e. Respresents Line subj id => Array (C.Attribute subj id _ state e)
linkA =
    [ Line.orientation $ Orientation.Vertical
    , Line.ch '≀'
    , Line.fg Palette.linkColor'
    ]


linkB :: forall subj id state e. Respresents Line subj id => Array (C.Attribute subj id _ state e)
linkB =
    [ Line.orientation $ Orientation.Horizontal
    , Line.type_ $ Border._bg
    , Line.ch '∼'
    , Line.fg Palette.linkColor'
    ]


linkC :: forall subj id state e. Respresents Line subj id => Array (C.Attribute subj id _ state e)
linkC =
    [ Line.orientation $ Orientation.Vertical
    , Line.type_ $ Border._bg
    , Line.ch '≀'
    , Line.fg Palette.linkColor'
    ]