module Example.HalogenVDom where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)


import Rpd (RpdError, Rpd, Network(..), emptyNetwork)
import Rpd (init) as Rpd
import Rpd.Render (Message(..), Renderer) as Ui
import Rpd.Render (update, once, make, make') as Render
import Rpd.Render.Terminal (terminalRenderer)
import Rpd.Render.Terminal (view) as TerminalRenderer


import FRP.Event as E

import Spork.Html (Html)
import Spork.Html as H


import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Thunk (Thunk, buildThunk)

import Effect (Effect, foreachE)
import Effect.Exception (throwException, error)
import Effect.Ref as Ref
import Effect.Uncurried as EFn

import Web.DOM.Element (toNode) as DOMElement
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (QuerySelector(..), querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML.Window (document) as DOM


type Model d = Network d
type Action d = Ui.Message d


render ∷ forall d. String → Html (Action d)
render src =
  H.div
    []
    [ H.button
        [ H.onClick (H.always_ Ui.Bang) ]
        [ H.text $ src ]
    ]


runVDom
    :: forall d r
     . String -- selector
    -> (r -> Html (Action d)) -- insert the rendering result
    -> Ui.Renderer d r -- renderer
    -> Network d -- initial network
    -> Effect Unit
runVDom sel render renderer initNw = do
    let sel = "#app"
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
                    , buildAttributes: P.buildProp (\a → push a)
                    }
            { first, next } <- Render.make {-event push-} initNw renderer
            vdom ← EFn.runEffectFn1 (V.buildVDom vdomSpec) (unwrap $ render first)
            void $ DOM.appendChild (Machine.extract vdom) (DOMElement.toNode el)
            cancel <- E.subscribe next $
                \v -> do
                    nextView <- v
                    -- FIXME: push `vdom` to the Ref
                    vdom ← EFn.runEffectFn2 Machine.step vdom (unwrap $ render nextView)
                    pure unit
            pure unit


main :: Effect Unit
main =
    runVDom "#app" render terminalRenderer $ emptyNetwork "foo"
