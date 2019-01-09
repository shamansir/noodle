module Rpd.Renderer.Html.Html where

import Data.Either (Either)
import Data.Tuple.Nested (type (/\), (/\))

import Rpd.API (RpdError) as R
import Rpd.Network (Network) as R
import Rpd.RenderMUV (Renderer(..), PushMsg, Message) as R

import Spork.Html (Html)
import Spork.Html as H


type Model =
    {
    }


data Msg
    = ClickAt (Int /\ Int)


type Message d = R.Message d Msg


type PushMsg d = R.PushMsg d Msg


type View = Html Msg


init :: Model
init =
    {
    }


type HtmlRenderer d = R.Renderer d Model View Msg


htmlRenderer :: forall d. HtmlRenderer d
htmlRenderer =
    R.Renderer
        { from : H.div [] []
        , init : init
        , update
        , view
        }


view :: forall d. PushMsg d -> Either R.RpdError (Model /\ R.Network d) -> View
view _ _ = H.div [] []


update :: forall d. Message d -> (Model /\ R.Network d) -> (Model /\ Array (Message d))
update _ (ui /\ _) =
    ui /\ []
