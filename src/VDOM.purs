module VDOM (render) where

import Prelude
import Data.CatList as CatList
import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree)
import Data.Array (fromFoldable)
import Data.List (head)
import Data.Maybe (Maybe)
import DOM (DOM)
import DOM.Node.Node as Node
import DOM.Node.Types (Element, Node, elementToNode, textToNode)
import Data.VirtualDOM (EventListener(On), with, text, h, VNode)
import Text.Smolder.Markup (EventHandler(EventHandler), MarkupM(..), Markup)
--import Text.Smolder.Renderer.Util (Node(Text, Element), renderMarkup)

type E eff = EventListener (dom :: DOM | eff)

-- renderNode :: ∀ e l v. Node (v → Eff e Unit) → VNode e l v
-- renderNode (Element name props CatList.CatNil children) =
--   h name props (renderNode <$> fromFoldable children)
-- renderNode (Element name props events children) =
--   let handlers = convertHandler <$> events
--       convertHandler (EventHandler name callback) = On name callback
--   in with (h name props (renderNode <$> fromFoldable children)) (fromFoldable handlers)
-- renderNode (Text t) = text t

render :: ∀ eff. Element → Markup (E eff) → Eff (dom :: DOM | eff) Unit
render target = foldFree (renderNode target)


renderNode :: ∀ foo eff. Element → MarkupM (E eff) foo → Eff (dom :: DOM | eff) foo
renderNode p (Element name children attrs CatList.CatNil rest) = do
  el ← h name attrs (renderNode <$> fromFoldable children)
  render el children
  _ ← Node.appendChild (elementToNode el) (elementToNode p)
  pure rest
renderNode p (Element name children attrs events rest) = do
  let handlers = convertHandler <$> events
      convertHandler (EventHandler name callback) = On name callback
  el ← with (h name attrs (renderNode <$> fromFoldable children)) (fromFoldable handlers)
  render el children
  _ ← Node.appendChild (elementToNode el) (elementToNode p)
  pure rest
renderNode p (Content t rest) = do
  textNode ← text t
  _ ← Node.appendChild (textToNode textNode) (elementToNode p)
  pure rest
renderNode p (Empty rest) = pure rest


-- TODO: patch (use the one from VDOM, just pass from markup?)

-- element :: ∀ eff. String → CatList Attr → CatList (EventHandler (E eff)) → Eff (dom :: DOM | eff) Element
-- element name attrs events = do
--   el ← makeElement name
--   setAttributes el attrs
--   traverse_ (setEvent el) events
--   pure el

-- Element String (Markup e) (CatList Attr) (CatList (EventHandler e)) a
--   | Content String a
--   | Empty a


-- | An example, using `Signal`, and assuming a `Signal (VNode e l v)`
-- | as input, and `api` from `Data.VirtualDOM.DOM`:
-- |
-- |     render :: ∀ e. Node → Signal (VNode e Node Event) → Eff (dom :: DOM | e) Unit
-- |     render target input =
-- |       runSignal $ withPrevious ~> patchDOM
-- |       where
-- |         withPrevious = foldp go (Tuple Nothing Nothing) input
-- |         go next (Tuple _ prev) = Tuple prev next
-- |         patchDOM (Tuple prev next) = patch api target prev next


-- | Update a DOM element's children in place.
-- |
-- | This will update existing nodes in place where possible,
-- | preserving their state. Other nodes will be created or deleted
-- | as appropriate.
-- |
-- | Please note that this function is currently not very smart -- it
-- | can't tell if a child node has moved inside its parent, and will
-- | not be able to reuse such nodes. (TODO)
-- patch :: ∀ eff. Element → Markup (E eff) → Eff (dom :: DOM | eff) Unit
-- patch parent markup = do
--   let node = elementToNode parent
--   children ← childrenOf node
--   childRef ← unsafeRunRef $ newRef children
--   foldFree (walk node childRef) markup
--   remainder ← unsafeRunRef $ readRef childRef
--   removeNodes node remainder
