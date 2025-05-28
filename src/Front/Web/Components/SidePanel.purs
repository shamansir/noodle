module Web.Components.SidePanel where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol, reflectSymbol)

import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T
import Data.Text.Output.Html as Html
import Data.String.CodeUnits as CU

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (style, ref) as HHP
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Web.Components.RawHtml as RawHTML

import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile
import Noodle.Ui.Tagging as T
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type SidePanel (id :: Symbol) s v =
    { title :: String
    , char :: v -> Char
    , next :: (s -> Effect (Array T.Tag))
    , value :: s -> v
    }


type Slots =
    ( rawHtml :: H.Slot (Const Void) Void Unit )


_rawHtml = Proxy :: _ "rawHtml"


type State s =
    { state :: s
    , tags :: Array T.Tag
    }


type ButtonState s v =
    { state :: s
    , value :: Maybe v
    }


data Action s
    = Initialize
    | Receive s


panel
    :: forall id input v query output m. IsSymbol id => MonadEffect m => Proxy id -> SidePanel id input v -> H.Component query input output m
panel pid config =
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
    initialState :: input -> State input
    initialState s =
        { state : s
        , tags : []
        }

    panelContentRef = H.RefLabel $ "panel-content" <> reflectSymbol pid

    render { tags } =
        HH.div
            [ ]
            [ HH.slot _rawHtml unit RawHTML.component { html: htmlText, elRef: panelContentRef } absurd
            ]
        where
            htmlText = Html.multiLine $ T.stack tags

    handleAction = case _ of
        Initialize -> do
            srec <- H.get
            tags <- H.liftEffect $ config.next srec.state
            H.put { tags, state : srec.state }
        Receive s -> do
            tags <- H.liftEffect $ config.next s
            H.put { tags, state : s }


charOf :: forall id s v. SidePanel id s v -> s -> Char
charOf { value, char } = char <<< value


-- FIXME: not used
button
    :: forall id input v query output m. MonadEffect m => SidePanel id input v -> H.Component query input output m
button config =
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
    initialState :: input -> ButtonState input v
    initialState s =
        { state : s
        , value : Nothing
        }

    panelSymbol = maybe '?' config.char

    render { value } =
        HS.text
            [ HSA.font_size $ HSA.FontSizeLength $ HSA.Px 14.0
            , HSA.fill $ Just $ P.hColorOf $ {- if Set.member which state.openPanels then _.i100 Palette.green else -} _.i100 Palette.blue
            , HSA.dominant_baseline HSA.BaselineMiddle
            ]
            [ HH.text $ CU.singleton $ panelSymbol value ]

    handleAction = case _ of
        Initialize -> do
            srec <- H.get
            let v = config.value srec.state
            H.put { value : Just v, state : srec.state }
        Receive s -> do
            let v = config.value s
            H.put { value : Just v, state : s }