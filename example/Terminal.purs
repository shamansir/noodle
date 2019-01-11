module Example.Terminal where

import Prelude

import Effect (Effect)

import Spork.Html (Html)
import Spork.Html as H

import Rpd.Network (Network)
import Rpd.Network (empty) as Network
import Rpd.Path (PatchId(..))
import Rpd.Command (Command(..)) as Cmd
import Rpd.RenderMUV (Message) as Ui
import Rpd.RenderMUV (core, custom)
import Rpd.Renderer.Terminal (terminalRenderer)
import Rpd.Renderer.Terminal.Multiline as ML
import Rpd.Renderer.Html.VDom as VDom

import Example.Network (network)
import Example.Toolkit (testPatch, testNode)


type Model d = Network d
type Action d msg = Ui.Message d msg


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
            (H.always_ $ core $ Cmd.AddPatch testPatch)
        ]
        [ H.text "Add Patch" ]
    , H.button
        [ H.onClick
            (H.always_ $ core $ Cmd.AddNode (PatchId 0) testNode)
        ]
        [ H.text "Add Node" ]
    ]


main :: Effect Unit
main =
    VDom.embed "#app" render terminalRenderer network
