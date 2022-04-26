module App.Layouts.App where

import Prelude

import Data.Tuple.Nested ((/\))

import App.Layout.Flex (Layers)
import App.Layout.Flex as F
import App.Layout.Flex.Rule (Rule)
import App.Layout.Flex.Rule as R


type Layout = Layers Rule AppLayoutPart


data AppLayoutPart
    = PatchTabs
    | PatchBackground
    | Body
    | NodeList
    | Space


instance showAppLayout :: Show AppLayoutPart where
    show PatchTabs = "patch tabs"
    show PatchBackground = "patch background"
    show Body = "body"
    show NodeList = "node list"
    show Space = "space"


layout :: Layout
layout =
    F.layers
        [ F.flex
            [ R.percents 15 /\
                [ R.fill /\ F.put PatchTabs
                ]
            , R.fill /\
                [ R.fill /\ F.put PatchBackground
                ]
            ]
        , F.flex
            [ R.percents 15 /\
                [ R.fill /\ F.put Space
                ]
            , R.fill /\
                [ R.fill /\ F.put Body
                ]
            ]
        ,  F.flex
            [ R.percents 15 /\
                [ R.fill /\ F.put Space
                ]
            , R.percents 5 /\
                [ R.fill /\ F.put Space
                ]
            , R.units 100.0 /\
                [ R.percents 5 /\ F.put Space
                , R.units 50.0 /\ F.put NodeList
                ]
            ]
        ]