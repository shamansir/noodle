module Web.Components.SidePanel where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T
import Data.Text.Output.Html as Html

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (style) as HHP

import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile
import Noodle.Ui.Tagging as T


type SidePanel (id :: Symbol) s v =
    { title :: String
    , char :: v -> Char
    , isOn :: v -> Boolean
    , next :: (s -> Effect (v /\ Array T.Tag))
    , onToggle :: (s -> s)
    }


type State s v =
    { state :: s
    , value :: Maybe v
    , tags :: Array T.Tag
    }


data Action s
    = Initialize
    | Receive s


panel
    :: forall id input v query output m. MonadEffect m => SidePanel id input v -> H.Component query input output m
panel config =
    H.mkComponent
        { initialState
        , render
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }
    where
    initialState :: input -> State input v
    initialState s =
        { state : s
        , value : Nothing
        , tags : []
        }

    maxWidth = 500.0

    render { tags } =
        HH.div
            [ HHP.style $ "display: block; text-align: right; position: absolute; top: 0; right: 0; max-width: " <> show maxWidth <> "px;" ]
            [ HH.text $ Html.multiLine $ T.stack tags ]

    handleAction = case _ of
        Initialize -> do
            srec <- H.get
            v /\ tags <- H.liftEffect $ config.next srec.state
            H.put { value : Just v, tags, state : srec.state }
        Receive s -> do
            v /\ tags <- H.liftEffect $ config.next s
            H.put { value : Just v, tags, state : s }