module Rpd.Renderer.Html.VDom
    (embed, embed') where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect.Exception (throwException, error)
import Effect.Ref as Ref
import Effect.Uncurried as EFn

import FRP.Event as E

import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Thunk (buildThunk)

import Rpd.RenderMUV (Renderer, Message) as Ui

import Web.DOM.Element (toNode) as DOMElement
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.ParentNode (querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML.Window (document) as DOM

import Spork.Html (Html)

import Rpd.API (Rpd) as R
import Rpd.Network (Network)
import Rpd.Command (Command(..)) as C
import Rpd.RenderMUV (custom, core)
import Rpd.RenderMUV (make') as Render


embed
    :: forall d model view msg
     . String -- selector
    -> (view -> Html (Ui.Message d msg)) -- insert the rendering result
    -> Ui.Renderer d model view msg -- renderer
    -> R.Rpd (Network d) -- initial network
    -> Effect Unit
embed sel render renderer initNw = do
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
            push $ core C.Bang
            pure unit


embed'
    :: forall d model msg
     . String -- selector
    -> Ui.Renderer d model (Html msg) msg -- renderer
    -> R.Rpd (Network d) -- initial network
    -> Effect Unit
embed'  sel renderer initNw =
    embed sel ((<$>) custom) renderer initNw
