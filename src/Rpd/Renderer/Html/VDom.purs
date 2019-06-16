module Rpd.Renderer.Html.VDom
    (embed, embed') where

import Prelude

import Effect (Effect)

import Data.Either (Either(..))
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
import Rpd.Toolkit (Toolkit) as T
import Rpd.Render.MUV (Renderer) as Ui
import Rpd.Render.MUV (make') as Render

import Debug.Trace as DT

type HtmlView msg cmd = Html (Either msg cmd)


-- TODO: it looks confusing, why embedding needs toolkits,
--       renderer indeed needs toolkits (which also looks confusing, but at least true)
embed
    :: forall d c n model view msg
     . String -- selector
    -> (view -> HtmlView msg (C.Command d c n)) -- insert the rendering result
    -> Ui.Renderer d c n model view msg -- renderer
    -> T.Toolkit d c n
    -> R.Rpd (Network d c n) -- initial network
    -> Effect Unit
embed sel render renderer toolkit initRpd = do
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
            let { first, next } = Render.make' { event, push } toolkit initRpd renderer
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
            push $ Right C.Bang
            pure unit


embed'
    :: forall d c n model msg
     . String -- selector
    -> Ui.Renderer d c n model (HtmlView msg (C.Command d c n)) msg -- renderer
    -> T.Toolkit d c n -- toolkits
    -> R.Rpd (Network d c n) -- initial network
    -> Effect Unit
embed' sel = embed sel identity
