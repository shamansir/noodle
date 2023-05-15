module Cli.Palette where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))

type Palette =
    { background :: String
    , background2 :: String
    , border :: String
    , familyMarker :: String
    , focusedBorder :: String
    , foreground :: String
    , itemNotSelected :: String
    , itemSelected :: String
    , linkColor :: String
    , nodeBoxBorder :: String
    , nodeListFg :: String
    , nodeListSelFg :: String
    }


palette :: Palette
palette =
    { background : "#111" -- 0
    , itemNotSelected : "#006600" -- 1
    , itemSelected : "#00ff00" -- 2
    , border : "#f0f0f0" -- 3
    , nodeListFg : "#666" -- 4
    , nodeListSelFg : "white" -- 5
    , nodeBoxBorder : "blue" -- 6
    , familyMarker : "#000033" -- 7
    , linkColor : "green" -- 8
    , focusedBorder : "white"
    , foreground : "white"
    , background2 : "black"
    }


toArray :: Palette -> Array (String /\ String)
toArray p =
    [ p.background /\ "background"
    , p.itemNotSelected /\ "itemNotSelected"
    , p.itemSelected /\ "itemSelected"
    , p.border /\ "border"
    , p.nodeListFg /\ "nodeListFg"
    , p.nodeListSelFg /\ "nodeListSelFg"
    , p.nodeBoxBorder /\ "nodeBoxBorder"
    , p.familyMarker /\ "familyMarker"
    , p.linkColor /\ "linkColor"
    , p.focusedBorder /\ "focusedBorder"
    , p.foreground /\ "foreground"
    , p.background2 /\ "background2"
    ]