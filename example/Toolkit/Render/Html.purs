module Example.Toolkit.Render.Html where

import Prelude (const, ($))

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- import Rpd.Toolkit (ToolkitRenderer)
import Rpd.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Rpd.API.Action.Sequence as A
import Rpd.Renderer.Html (View, ToolkitRenderer, core) as R
import Rpd.Path as P

import Spork.Html (Html)
import Spork.Html as H

import Example.Toolkit.Nodes
import Example.Toolkit.Value
import Example.Toolkit.Channel


-- type RenderNode d msg view = forall msg. R.Node d -> (msg -> Effect Unit) -> view
-- type RenderInlet c d msg view = Channels d c => (R.Inlet d -> c -> (msg -> Effect Unit) -> view)
-- type RenderOutlet c d msg view = Channels d c => (R.Outlet d -> c -> (msg -> Effect Unit) -> view)


-- data Renderer msg d c view = Renderer
--     { node :: NodeDefAlias /-> RenderNode msg d view
--     , inlet :: ChannelDefAlias /-> RenderInlet c msg d view
--     , outlet :: ChannelDefAlias /-> RenderOutlet c msg d view
--     }


-- type Command = R.Command Value Channel Node
-- type View = R.View Value Channel Node


renderer :: R.ToolkitRenderer Value Channel Node
renderer =
    { renderNode : \_ _ _ ->
        H.div
            [ H.classes [ "tk-node" ] ]
            [ H.text "tk-node"
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.Request $ A.ToAddPatch "test" ]
                [ H.text "ADD PATCH" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.addNode (P.toPatch "test") "random1" RandomNode ]
                [ H.text "ADD RANDOM1 NODE" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.addNode (P.toPatch "test") "random2" RandomNode ]
                [ H.text "ADD RANDOM2 NODE" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.addInlet (P.toNode "test" "random1") "test" NumberChannel ]
                [ H.text "ADD INLET TEST TO RANDOM1" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.addInlet (P.toNode "test" "random1") "foo" NumberChannel ]
                [ H.text "ADD INLET FOO TO RANDOM1" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.addInlet (P.toNode "test" "random2") "test" NumberChannel ]
                [ H.text "ADD INLET TEST TO RANDOM2" ]
            -- , H.div
            --     [ H.onClick $ H.always_ $ R.core
            --         $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random1" "min") $ Shape Cross ]
            --     [ H.text "SEND DATA TO MIN" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random1" "test") $ Shape Diamond ]
                [ H.text "SEND DATA TO RANDOM1/TEST" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random1" "foo") $ Shape Diamond ]
                [ H.text "SEND DATA TO RANDOM1/FOO" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.Request $ A.ToSendToInlet (P.toInlet "test" "random2" "test") $ Shape Diamond ]
                [ H.text "SEND DATA TO RANDOM2/TEST" ]
            , H.div
                [ H.onClick $ H.always_ $ R.core
                    $ A.Request
                    $ A.ToSendPeriodicallyToInlet (P.toInlet "test" "random1" "test") 500
                    $ const
                    $ Shape Diamond ]
                [ H.text "SEND DATA TO RANDOM1/TEST PERIOD" ]
            ]
    , renderInlet : \_ _ _ -> H.div [ H.classes [ "tk-inlet" ] ] [ H.text "tk-inlet" ]
    , renderOutlet : \_ _ _ -> H.div [ H.classes [ "tk-outlet" ] ] [ H.text "tk-outlet" ]
    }
