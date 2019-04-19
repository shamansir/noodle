module Example.Terminal where

import Prelude

import Data.List (List(..)) as List
import Effect (Effect)

import Spork.Html (Html)
import Spork.Html as H

import Rpd.Network (Network)
import Rpd.Network (empty) as Network
import Rpd.Def as R
import Rpd.Process as R
import Rpd.Path (PatchId(..))
import Rpd.Command (Command(..)) as Cmd
import Rpd.Render.MUV (Message) as Ui
import Rpd.Render.MUV (core, custom)
import Rpd.Renderer.Terminal (terminalRenderer)
import Rpd.Renderer.Terminal.Multiline as ML
import Rpd.Renderer.Html.VDom as VDom


type Model d = Network d
type Action d msg = Ui.Message d msg


patch :: forall d. R.PatchDef d
patch =
    { name : "patch"
    , nodeDefs : List.Nil
    }


node :: forall d. R.NodeDef d
node =
    { name : "node"
    , inletDefs : List.Nil
    , outletDefs : List.Nil
    , process : R.Withhold
    }


render ∷ forall d msg. ML.Multiline → Html (Action d msg)
render src =
  H.div
    []
    [ H.textarea
        [ H.style $
            "width: 500px; height: 500px;" <>
            "font-family: monospace; font-size: 14px;" <>
            "outline: none; border: none;"
        , H.value
            $ show src
        ]
    , H.button
        [ H.onClick
            (H.always_ $ core Cmd.Bang)
        ]
        [ H.text "Bang" ]
    , H.button
        [ H.onClick
            (H.always_ $ core $ Cmd.AddPatch patch)
        ]
        [ H.text "Add Patch" ]
    , H.button
        [ H.onClick
            (H.always_ $ core $ Cmd.AddNode (PatchId 0) node)
        ]
        [ H.text "Add Node" ]
    ]


main :: Effect Unit
main =
    VDom.embed "#app" render terminalRenderer $ pure $ Network.empty "foo"
