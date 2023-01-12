module Web.App.Layouts.App where

import Prelude

import Web.App.Layout.Flex (Layers)
import Web.App.Layout.Flex as F
import Web.App.Layout.Flex.Rule (Rule)
import Web.App.Layout.Flex.Rule as R
import Data.Tuple.Nested ((/\))


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
            [ topRule /\
                [ R.fill /\ F.put PatchTabs
                ]
            , R.fill /\
                [ R.fill /\ F.put PatchBackground
                ]
            ]
        , F.flex
            -- -- R.min 150.0 (R.percents 10) /\
            [ topRule /\
                [ R.fill /\ F.put Space
                ]
            , R.fill /\
                [ R.fill /\ F.put Body
                ]
            ]
        ,  F.flex
            [ topRule /\
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
    where
        topRule = R.min 25.0 $ R.percents 7