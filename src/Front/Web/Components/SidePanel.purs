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
import Halogen.HTML.Properties.Extra (Position(..), position) as HHP
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Web.Paths as Paths
import Web.Layer (TargetLayer(..))
import Web.Components.RawHtml as RawHTML

import Front.Shared.Bounds (Size)

import Noodle.Text.NdfFile (toNdfCode, toTaggedNdfCode) as NdfFile
import Noodle.Ui.Tagging as T
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type RenderParams =
    { size :: Size
    }


type SidePanel (id :: Symbol) s v =
    { title :: String
    , char :: v -> Char
    , next :: (s -> Effect (Array T.Tag))
    , value :: s -> v
    }


type Slots =
    ( rawHtml :: H.Slot (Const Void) Void Unit )


_rawHtml = Proxy :: _ "rawHtml"


type Input s =
    RenderParams /\ s


type State s =
    { state :: s
    , tags :: Array T.Tag
    , size :: Size
    }


type ButtonState s v =
    { state :: s
    , value :: Maybe v
    }


data Action s
    = Initialize
    | Receive s


slopeFactor = 5.0 :: Number
fontSize = 12.0 :: Number
headerHeight = 20.0 :: Number
contentPadding = 20.0 :: Number

panel
    :: forall id s v query output m
     . IsSymbol id
    => MonadEffect m
    => TargetLayer
    -> Proxy id
    -> SidePanel id s v
    -> H.Component query (Input s) output m
panel targetLayer pid config =
    H.mkComponent
        { initialState
        , render : render targetLayer
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }
    where
    initialState :: Input s -> State s
    initialState ({ size } /\ s) =
        { state : s
        , tags : []
        , size
        }

    panelContentRef = H.RefLabel $ "panel-content" <> reflectSymbol pid

    render HTML { tags } =
        HH.div
            [ HHP.position HHP.Rel { x : contentPadding, y : headerHeight + contentPadding } ]
            [ HH.slot _rawHtml unit RawHTML.component { html: htmlText, elRef: panelContentRef } absurd
            ]
        where
            htmlText = Html.multiLine $ T.stack tags

    render SVG { tags, size } =
        HS.g
            [ ]
            [ backdrop
            ]
        where
            width  = size.width
            height = size.height
            backdrop =
                HS.g
                    [ HSA.transform [ HSA.Translate 0.0 0.0 ] ]
                    [ HS.path
                        [ HSA.d $ Paths.panelTop { slope : slopeFactor, width, height : headerHeight }
                        , HSA.fill   $ Just $ P.hColorOf $ _.i900 Palette.magenta
                        , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.magenta
                        , HSA.strokeWidth 1.0
                        ]
                    , HS.text
                        [ HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                        , HSA.x 6.0
                        , HSA.y 7.0
                        , HSA.font_size $ HSA.FontSizeLength $ HSA.Px fontSize
                        , HSA.dominant_baseline HSA.Hanging
                        ]
                        [ HH.text $ ": " <> config.title ]
                    , HS.g
                        [ HSA.transform [ HSA.Translate 0.0 headerHeight ] ]
                        [ HS.path
                            [ HSA.d $ Paths.panelBody { slope : slopeFactor, width, height : height - headerHeight }
                            , HSA.fill   $ Just $ P.hColorOf $ _.i900 Palette.blue
                            , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.blue
                            , HSA.strokeWidth 1.0
                            ]
                        ]
                    ]

    handleAction = case _ of
        Initialize -> do
            srec <- H.get
            tags <- H.liftEffect $ config.next srec.state
            H.put { tags, state : srec.state, size : srec.size }
        Receive ({ size } /\ s) -> do
            tags <- H.liftEffect $ config.next s
            H.put { tags, state : s, size }


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