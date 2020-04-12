module Example.Toolkit.Html.OnHtml where

import Prelude

import Data.Vec2 (Vec2(..))
import Data.Vec2 as Vec2
import Data.Spread (Spread(..)) as S
import Data.Spread as Spread
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (take) as String
import Data.Array (catMaybes, length, mapWithIndex) as Array
import Data.Traversable (for_)

-- import Rpd.Render.Html (View) as R
import Spork.Html (Html)
import Spork.Html as H

import Example.Toolkit.Value
import Example.Toolkit.Value (Value(..)) as V


class OnHtml x where
    -- html :: forall c n. x -> R.View x c n
    html :: forall msg. x -> Html msg


instance drawOnHtml :: OnHtml DrawOp where
    html (Ellipse dimensions) = do
        H.div
            [ H.classes [ "tk-value-shape" ]
            ]
            [ H.text "◯"
            , renderDimensions dimensions
            ]
    html (Rect dimensions) = do
        H.div
            [ H.classes [ "tk-value-shape" ]
            ]
            [ H.text "▭"
            , renderDimensions dimensions
            ]


instance styleOnHtml :: OnHtml StyleOp where
    html (Fill fill) =
        H.div
            [ H.classes [ "tk-value-fill" ]
            ]
            [ H.text "fill:"
            , renderColor fill
            ]
    html (Stroke color w) = do
        H.div
            [ H.classes [ "tk-value-stroke" ]
            ]
            [ H.text "stroke:("
            , renderColor color
            , H.text $ ", " <> show w <> ")"
            ]


instance transformOnHtml :: OnHtml TransformOp where
    html (Move vec) =
        H.div
            [ H.classes [ "tk-value-transform" ]
            ]
            [ H.text "move: "
            , renderDirection vec
            ]
    html (Scale vec) = do
        H.div
            [ H.classes [ "tk-value-transform" ]
            ] [ H.text "scale: "
            , renderDimensions vec
            ]


instance instructionOnHtml :: OnHtml Instruction where
    html NoOp = H.text "(NoOp)"
    html (Draw draw) =
        H.div
            [ H.classes [ "tk-value-instruction" ]
            ]
            [ H.text "🖌️"
            , html draw
            ]
    html (Style style) =
        H.div
            [ H.classes [ "tk-value-instruction" ]
            ]
            [ H.text "👁️"
            , html style
            ]
    html (Transform transform) =
        H.div
            [ H.classes [ "tk-value-instruction" ]
            ]
            [ H.text "🌐"
            , html transform
            ]


instance valueOnHtml :: OnHtml Value where
    html Bang =
        H.div [] [ H.text "◌" ]
    html (Numerical n) =
        H.div [] [ H.text $ String.take 5 $ show n ]
    html (Vector vec) =
        renderCoords vec
    html (V.Color color) = do
        renderColor color
    html (Apply instruction) =
        html instruction
    html (Pair valA valB) = do
        H.div
            [ H.classes [ "tk-value-pair" ]
            ]
            [ html valA
            , html valB
            ]
    html (Spread spread) =
        H.div
            [ ]
            $ html <$> (Array.catMaybes $ Spread.run spread)


renderIfSet :: forall msg. String -> Maybe Value -> Html msg
renderIfSet _ (Just value) = html value
renderIfSet defaultText Nothing = H.text defaultText


renderIfSetAnd :: forall msg. (Value -> Boolean) -> String -> Maybe Value -> Html msg
renderIfSetAnd check _ (Just value) | check value = html value
renderIfSetAnd _ defaultText (Just value) | otherwise = H.text defaultText
renderIfSetAnd _ defaultText _ = H.text defaultText


isSpread :: Value -> Boolean
isSpread (Spread _) = true
isSpread _ = false


renderIfColor :: forall msg. Maybe Value -> Html msg
renderIfColor (Just (Color color)) = renderColor color
renderIfColor _ = renderNoColor


renderColor :: forall msg. RgbaColor -> Html msg
renderColor color =
    H.div
        [ H.classes [ "tk-color-value" ]
        , H.style $ "background-color: " <> colorToCss color <> ";"
        ]
        [ ]


renderNoColor :: forall msg. Html msg
renderNoColor =
    H.div
        [ H.classes [ "tk-color-value" ]
        ]
        [ H.text "?" ]


renderDirection :: forall msg. Vec2 -> Html msg
renderDirection vec =
    H.div
        [ H.classes [ "tk-direction-value" ]
        , H.style $ "transform: rotate(" <> (Vec2.arrow (Vec2 0.0 0.0 /\ vec) # _.angle # show) <> "rad);"
        ]
        [ H.text "⟶" ]


renderCoords :: forall msg. Vec2 -> Html msg
renderCoords (Vec2 x y) =
    H.div
        [ H.classes [ "tk-coords-value" ]
        ]
        [ H.text $ show x <> ":" <> show y ]


renderDimensions :: forall msg. Vec2 -> Html msg
renderDimensions (Vec2 w h) =
    H.div
        [ H.classes [ "tk-dimensions-value" ]
        ]
        [ H.text $ show w <> "x" <> show h ]


renderSpread :: forall msg. S.Spread Value -> Html msg
renderSpread spread =
    let
        maxItems = 10
        arr = Array.catMaybes $ Spread.run spread
        len = Array.length arr
    in case arr of
        [] -> H.div [] []
        items | len <= maxItems ->
            Array.mapWithIndex (/\) items
                # map (uncurry renderItemWithIndex)
                # H.div [ H.classes [ "tk-spread-value" ] ]
        aLotOfItems ->
            Array.mapWithIndex (\idx item ->
                if idx `mod` (floor (toNumber len / toNumber maxItems)) == 0 then
                    Just $ idx /\ item
                else Nothing
            ) arr
                # Array.catMaybes
                # map (uncurry renderItemWithIndex)
                # H.div [ H.classes [ "tk-spread-value" ] ]
    where
        renderItemWithIndex index item =
            H.div []
                [ H.text $ show index <> ":"
                , renderItem item
                ]
        renderItem (Numerical num) = H.text $ show num
        renderItem (Color color) = renderColor color
        renderItem (Vector vec) = renderDirection vec
        renderItem (Pair val1 val2) =
            H.div [ H.classes [ "tk-pair-value" ] ]
                [ renderItem val1
                , renderItem val2
                ]
        renderItem _ = H.div [] []
