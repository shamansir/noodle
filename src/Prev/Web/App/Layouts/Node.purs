module Layouts.Node
  ( layout, NodeLayoutPart
  )
  where

import Prelude


import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Layout.Flex (Flex)
import Layout.Flex as F
import Layout.Flex.Rule (Rule)
import Layout.Flex.Rule as R


data NodeLayoutPart
    = Title
    | CloseButton
    | Body
    | Inlet Int
    | Outlet Int
    | Space


layout :: Int -> Int -> Flex Rule NodeLayoutPart
layout inletsCount outletsCount =
    F.flex
        [ R.units 30.0 /\
            [ R.units 30.0 /\ F.put Space -- padding-left
            , R.fill /\
                (F.nest'
                    $ F.flex1 R.fill
                        [ R.fill /\ F.put Title, R.units 10.0 /\ F.put CloseButton ]
                )
            , R.units 30.0 /\ F.put Space -- padding-right
            ]
        , R.fill /\ -- vert
            [ R.units 30.0 /\ -- horz
                F.nest2' R.fill
                    (F.flex
                        [ R.units 5.0 /\ [ R.fill /\ F.put (Inlet 1) ]
                        , R.units 5.0 /\ [ R.fill /\ F.put (Inlet 2) ]
                        , R.units 5.0 /\ [ R.fill /\ F.put (Inlet 3) ]
                        , R.fill /\ [ R.fill /\ F.put Space ]
                        ]
                    )
            , R.fill /\ F.put Body
            , R.units 30.0 /\ -- horz
                F.nest2' R.fill
                    (F.flex
                        [ R.fill /\ [ R.fill /\ F.put Space ]
                        , R.units 5.0 /\ [ R.fill /\ F.put (Outlet 1) ]
                        , R.units 5.0 /\ [ R.fill /\ F.put (Outlet 2) ]
                        ]
                    )
            ]
        ]
