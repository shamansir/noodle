module RayDraw.Toolkit.Node where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Noodle.Process (ProcessF(..)) as R
import Noodle.Toolkit ((~<), (>~))
import Noodle.Toolkit (NodeDef(..), noInlets, noOutlets, withInlets, withOutlets, defineNode) as T
import RayDraw.Toolkit.Channel (Channel(..))
import RayDraw.Toolkit.Value (Value(..), Aggregate(..))
import Noodle.Render.Atom (class Atom) as R


type NodeDef = T.NodeDef Value Channel


data Node
    = BangNode
    | NodeListNode
    | PaletteNode
    | RayNode
    | DrawLogoNode
    | PreviewNode



nodesForTheList :: Array Node
nodesForTheList =
    [ BangNode,
      PaletteNode,
      RayNode,
      DrawLogoNode,
      PreviewNode
    ]


instance showNode :: Show Node where
    show BangNode = "bang"
    show NodeListNode = "node list"
    show PaletteNode = "palette"
    show RayNode = "ray"
    show DrawLogoNode = "draw logo"
    show PreviewNode = "preview"



{- BANG NODE -}

bangNode :: NodeDef
bangNode =
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "bang" /\ Channel)
        $ R.Withhold

{- PALETTE NODE -}

paletteNode :: NodeDef
paletteNode =
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "color" /\ Channel)
        $ R.Withhold

{- RAY NODE -}

rayNode :: NodeDef
rayNode =
    T.defineNode
        (T.withInlets
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "bang" /\ Channel)
        $ R.Withhold

{- DRAW LOGO NODE -}

drawLogoNode :: NodeDef
drawLogoNode =
    T.defineNode
        (T.withInlets
            ~< "color" /\ Channel
            ~< "bang" /\ Channel)
        (T.withOutlets
            >~ "logo" /\ Channel)
        $ R.Process
            $ \receive ->
                let
                    send "color" = receive "logo"
                    send _ = Nothing
                in pure send

{- PREVIEW NODE -}

allOfNode :: NodeDef
allOfNode =
    T.defineNode
        (T.withInlets
            ~< "image" /\ Channel)
        (T.withOutlets
            >~ "image" /\ Channel)
        $ R.Withhold
           
instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
