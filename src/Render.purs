module Render
    ( renderer
    , Event(..)
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.Event as DOM
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Foldable (class Foldable, for_, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\), type (/\))
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM as ToDOM


newtype UIState =
    UIState
        { dragging :: Maybe R.NodePath
        , connecting :: Maybe R.OutletPath
        }


data Event
    = Start
    | Connect String String
    | Drag Int Int

-- type Updates d e = R.Updates (UIState d) d (dom :: DOM | e )


data UI d = UI UIState (R.Network d)


type UIChannel d = SC.Channel (UI d)


type Markup e = H.Markup (EventListener ( channel :: SC.CHANNEL, dom :: DOM | e ))


type DomRenderer d e = R.Renderer d ( dom :: DOM | e )


initState :: UIState
initState =
    UIState
        { dragging : Nothing
        , connecting : Nothing
        }


initUi :: forall d e. R.Network d -> Eff (channel :: SC.CHANNEL | e) (UIChannel d)
initUi nw =
    SC.channel (UI initState nw)


renderer
    :: forall d e
     . Element
    -> R.Network d
    -> Eff (channel :: SC.CHANNEL | e)
           (S.Signal (Eff ( channel :: SC.CHANNEL, dom :: DOM | e ) Unit))
-- renderer :: forall d e. Element -> DomRenderer d e
renderer target nw = do
    --nwChannel <- SC.channel nw
    -- let nwSignal = SC.subscribe nwChannel
    -- stateChannel <- SC.channel initState
    uiChannel <- SC.channel (UI initState nw)
    let uiSignal = SC.subscribe uiChannel
    -- TODO: let mergedSignal = S.sampleOn
    --ToDOM.patch target (network ui ch)
    --let update = \ui _ -> update ui uiChannel
    -- liftEff $ uiSignal S.~> (\ui -> render target ui uiChannel)
    pure $ uiSignal S.~> (\ui -> render target ui uiChannel)
    -- S.foldp update (UI initState nw) uiSignal


-- { init :: Network d0
--               -> Eff
--                    ( channel :: CHANNEL
--                    | e1
--                    )
--                    (Channel (UI (UIState d0) d0))
--     , update :: UI (UIState d0) d0
--                 -> Channel (UI (UIState d0) d0)
--                    -> Eff
--                         ( channel :: CHANNEL
--                         | e1
--                         )
--                         Unit
--     }


render
    :: forall d e
     . Element
    -> UI d
    -> UIChannel d
    -> Eff ( channel :: SC.CHANNEL, dom :: DOM | e ) Unit
render target ui ch = do
    ToDOM.patch target (network ui ch)

    --ToDOM.render target (network ui ?what)
    -- let state = initState
    -- let ui = state /\ nw
    -- channel <- SC.channel nw
    -- let signal = SC.subscribe channel
    -- let sender = (\network -> do SC.send channel network)
    -- S.runSignal (signal S.~> (\network -> render target $ network network sender))


network :: forall d e. UI d -> UIChannel d -> Markup e
network ui@(UI _ (R.Network patches)) ch =
    H.div $ do
        H.p $ H.text "Network"
        H.p $ H.text $ "\tHas " <> (show $ length patches) <> " Patches"
        for_ patches (\p -> patch ui ch p)


patch :: forall d e. UI d -> UIChannel d -> R.Patch d -> Markup e
patch ui ch (R.Patch patchId label nodes links) =
    H.div $ do
        H.p $ H.text $ "Patch: " <> label <> " " <> show patchId
        H.p $ H.text $ "\tHas " <> (show $ length nodes) <> " Nodes"
        H.p $ H.text $ "\tHas " <> (show $ length links) <> " Links"
        for_ nodes (\n -> node ui ch n)


node :: forall d e. UI d -> UIChannel d -> R.Node d -> Markup e
node ui ch (R.Node path name inlets outlets _) =
    H.div $ do
        H.p $ H.text $ "Node: " <> name <> " " <> show path
        H.p $ H.text $ "\tHas " <> (show $ length inlets) <> " Inlets"
        H.p $ H.text $ "\tHas " <> (show $ length outlets) <> " Outlets"
        for_ inlets (\i -> inlet ui ch i)
        for_ outlets (\o -> outlet ui ch o)


inlet :: forall d e. UI d -> UIChannel d -> R.Inlet d -> Markup e
inlet ui@(UI s _) ch (R.Inlet path label _) =
    H.div $ do
        H.p #! on "click" (eventListener $ transformWith ch evt ui) $ H.text $ "Inlet: " <> label <> " " <> show path
    where
        evt = Connect "foo" "bar"


outlet :: forall d e. UI d -> UIChannel d -> R.Outlet d -> Markup e
outlet ui ch (R.Outlet path label _) =
    H.div $ do
        H.p $ H.text $ "Outlet: " <> label <> " " <> show path


--transformWith :: Event ->
transformWith ch evt ui = (\_ -> SC.send ch $ update evt ui)


-- TODO: Add UIState in the loop
update :: forall d e. Event -> UI d -> UI d
update evt (UI state network) =
    UI state network
    -- case evt of
    --     Start -> H.p $ H.text $ "Start"
    --     Connect s1 s2 -> H.p $ H.text $ "Connect: " <> s1 <> s2
    --     Drag i1 i2 -> H.p $ H.text $ "Drag: " <> show i1 <> show i2


-- TODO: render :: forall d e. Network d -> Eff ( dom :: DOM | e ) Unit
