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

import Rpd.Network (Network)
import Rpd.API.Action (Action(..), DataAction(Bang), RequestAction(ToAddPatch)) as A
import Rpd.Toolkit (Toolkit) as T
import Rpd.Render.MUV (Renderer) as Ui
import Rpd.Render.MUV (make, PushF(..)) as Render

import Debug.Trace as DT

type HtmlView d c n action = Html (Either action (A.Action d c n))


-- TODO: it looks confusing, why embedding needs toolkits,
--       renderer indeed needs toolkits (which also looks confusing, but at least true)
embed
    :: forall d c n model view action effect
     . String -- selector
    -> (view -> HtmlView d c n action) -- insert the rendering result
    -> Ui.Renderer d c n model view action effect -- renderer
    -> T.Toolkit d c n
    -> Network d c n -- initial network
    -> Effect Unit
embed sel render renderer toolkit nw = do
    doc ← DOM.window >>= DOM.document
    mbEl ← DOM.querySelector (wrap sel) (HTMLDocument.toParentNode doc)
    case mbEl of
        Nothing -> throwException (error ("Element does not exist: " <> sel))
        Just el -> do
            { first, next, push } <- Render.make renderer toolkit nw
            let
                Render.PushF pushF = push
                vdomSpec = V.VDomSpec
                    { document : HTMLDocument.toDocument doc
                    , buildWidget: buildThunk unwrap
                    , buildAttributes: P.buildProp pushF
                    }
            first_vdom ← EFn.runEffectFn1 (V.buildVDom vdomSpec) (unwrap $ render first)
            vdom_ref <- Ref.new first_vdom -- use recursion istead of `Ref`?
            void $ DOM.appendChild (Machine.extract first_vdom) (DOMElement.toNode el)
            cancel <- E.subscribe next $
                \next_view -> do
                    prev_vdom <- Ref.read vdom_ref
                    next_vdom ← EFn.runEffectFn2 Machine.step prev_vdom (unwrap $ render next_view)
                    _ <- Ref.write next_vdom vdom_ref
                    pure unit
            _ <- pushF $ Right $ A.Data A.Bang
            _ <- pushF $ Right $ A.Request $ A.ToAddPatch "bar"
            pure unit


embed'
    :: forall d c n model action effect
     . String -- selector
    -> Ui.Renderer d c n model (HtmlView d c n action) action effect -- renderer
    -> T.Toolkit d c n -- toolkits
    -> Network d c n -- initial network
    -> Effect Unit
embed' sel = embed sel identity
