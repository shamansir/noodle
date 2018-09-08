module Example.HalogenVDom where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.List as List

import FRP.Event as E

import Spork.Html (Html)
import Spork.Html as H


import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Thunk (Thunk, buildThunk)

import Effect (Effect)
import Effect.Exception (throwException, error)
import Effect.Ref as Ref
import Effect.Uncurried as EFn

import Web.DOM.Element (toNode) as DOMElement
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (QuerySelector(..), querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML.Window (document) as DOM

import Rpd.Def (ProcessF(..))
import Rpd.Network (Network)
import Rpd.Network (empty) as Network
import Rpd.Path (PatchId(..))
import Rpd.Def (NodeDef, PatchDef) as Rpd
import Rpd.Render (Message(..)) as Ui
import Rpd.RenderS (Renderer) as Ui
import Rpd.RenderS (make') as Render
import Rpd.Renderer.Terminal (terminalRenderer)



type Model d = Network d
type Action d = Ui.Message d


testPatch :: forall d. Rpd.PatchDef d
testPatch =
    { name : "foo"
    , nodeDefs : List.Nil
    }


testNode :: forall d. Rpd.NodeDef d
testNode =
    { name : "a"
    , inletDefs : List.Nil
    , outletDefs : List.Nil
    , process : FlowThrough
    }


render ∷ forall d. String → Html (Action d)
render src =
  H.div
    []
    [ H.button
        [ H.onClick
            (H.always_ $ Ui.AddNode (PatchId 0) testNode)
        ]
        [ H.text "Add Node" ]
    , H.button
        [ H.onClick
            (H.always_ $ Ui.AddPatch testPatch)
        ]
        [ H.text "Add Patch" ]
    , H.text src
    ]


-- import Halogen.VDom.DOM.Prop as P
-- import Halogen.VDom as V
-- import Halogen.VDom.Thunk (Thunk, thunk1, thunk2, thunk3, thunked)
-- type HtmlV i = V.VDom (Array (P.Prop i)) (Thunk Html i)
-- newtype Html i = Html (HtmlV i)
-- derive instance newtypeHtml ∷ Newtype (Html i) _
runVDom
    :: forall d model view
     . String -- selector
    -> (view -> Html (Action d)) -- insert the rendering result
    -> Ui.Renderer d model view -- renderer
    -> Network d -- initial network
    -> Effect Unit
runVDom sel render renderer initNw = do
    doc ← DOM.window >>= DOM.document
    mbEl ← DOM.querySelector (wrap sel) (HTMLDocument.toParentNode doc)
    case mbEl of
        Nothing -> throwException (error ("Element does not exist: " <> sel))
        Just el -> do
            { event, push } <- E.create
            let
                vdomSpec = V.VDomSpec
                    { document : HTMLDocument.toDocument doc
                    , buildWidget: buildThunk unwrap
                    , buildAttributes: P.buildProp push
                    }
            let { first, next } = Render.make' { event, push } initNw renderer
            first_vdom ← EFn.runEffectFn1 (V.buildVDom vdomSpec) (unwrap $ render first)
            vdom_ref <- Ref.new first_vdom -- use recursion istead of `Ref`?
            void $ DOM.appendChild (Machine.extract first_vdom) (DOMElement.toNode el)
            cancel <- E.subscribe next $
                \v -> do
                    next_view <- v
                    prev_vdom <- Ref.read vdom_ref
                    next_vdom ← EFn.runEffectFn2 Machine.step prev_vdom (unwrap $ render next_view)
                    _ <- Ref.write next_vdom vdom_ref
                    pure unit
            pure unit


main :: Effect Unit
main =
    runVDom "#app" render terminalRenderer $ Network.empty "foo"
