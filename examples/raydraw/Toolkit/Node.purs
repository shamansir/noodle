module RayDraw.Toolkit.Node where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Console (log, logShow)
import Noodle.Process (ProcessF(..)) as R
import Noodle.Render.Atom (class Atom) as R
import Noodle.Toolkit ((~<), (>~))
import Noodle.Toolkit (NodeDef, defineNode, withInlets, withOutlets) as T
import RayDraw.Toolkit.Channel (Channel(..))
import RayDraw.Toolkit.Render (renderNativeRay, renderRay)
import RayDraw.Toolkit.Value (Value)

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
        $ R.Process pure


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

previewNode :: NodeDef
previewNode =
    T.defineNode
        (T.withInlets
            ~< "image" /\ Channel)
        (T.withOutlets
            >~ "image" /\ Channel)
        $ R.Process
            $ \receive ->                
                do 
                  log "test test"
                  renderRay 1.0
                  pure $ \s -> Nothing


           
instance nodeAtom :: R.Atom Node where
    labelOf = show
    uniqueIdOf = show
    debugInfoOf = show
