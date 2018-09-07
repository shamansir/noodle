module Example.HalogenVDom where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)


import Rpd (RpdError, Rpd, Network(..), emptyNetwork)
import Rpd (init) as Rpd
import Rpd.Render (Message(..)) as Ui
import Rpd.Render (update, once, Renderer, proxy', run') as Render
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

renderer :: forall d. Render.Renderer d (Html (Action d))
renderer =
  terminalRenderer
    # Render.proxy'
      (const $ H.div [] [ H.text "error"])
      inject
      (H.div [] [])
  where
    inject :: (Ui.Message d -> Effect Unit) -> String -> Html (Action d)
    inject pushMsg strView =
      H.div [] []


render ∷ forall d. Model d → Html (Action d)
render (Network def _) =
  H.div
    []
    [ H.button
        [ H.onClick (H.always_ Ui.Bang) ]
        [ H.text $ "Hit me (" <> def.name <> ")" ]
    ]

main = do
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
                initNw = emptyNetwork "foo"
            initRender <- Render.run' event push initNw renderer
            vdom ← EFn.runEffectFn1 (V.buildVDom vdomSpec) (unwrap initRender)
            void $ DOM.appendChild (Machine.extract vdom) (DOMElement.toNode el)

            -- //// TODO: perform on every render result
            vdom ← EFn.runEffectFn2 Machine.step vdom (unwrap initRender)
            pure unit
            -- pure $ state { vdom = vdom, status = Flushed }
