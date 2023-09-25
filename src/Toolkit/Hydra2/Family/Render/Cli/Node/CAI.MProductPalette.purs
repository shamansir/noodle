module Toolkit.Hydra2.Family.Render.Cli.Node.CAI.FProductPalette where

import Prelude

import Prim.Symbol (class Append) as S

import Type.Proxy (Proxy)
import Data.Number as Number
import Data.Array (range, foldl, (:))
import Data.Traversable (traverse)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

import Data.Maybe (Maybe(..))

import Signal (Signal)

import Blessed as B
import Blessed ((>~), (~<))
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.BlessedSubj (Button)
import Blessed.Internal.NodeKey (type (<^>), type (<<>>))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift) as Blessed
import Blessed.Internal.NodeKey as NodeKey

import Blessed.Core.Border as Border
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Coord as C
import Blessed.Core.Coord ((<->))

import Blessed.Internal.Core as Core

import Blessed.UI.Base.Node.Method (append) as Node
import Blessed.UI.Boxes.Box.Option (content, height, left, top, width, tags) as Box
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Boxes.Box.Method (setContent) as Box


import Noodle.Node2 (sendInM) as Node

import Cli.Keys (NodeBoxKey)
import Cli.Style as Style

import CompArts.Product as CAI

-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Types as T
-- import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, Node)
import Toolkit.Hydra2.Family.CAI.FProductPalette (Node, State, _in_product) as FProductPalette
-- import Toolkit.Hydra2 (State) as Hydra

type ButtonKey = Button <^> "product-palette-text-box"


type NestedButtonKey = Button <^> "node-box::product-palette-text-box" -- TODO: use NK.NKAppend


render :: forall m. Applicative m => MonadEffect m => NodeBoxKey -> FProductPalette.Node m -> BlessedOp FProductPalette.State m
render nodeBoxKey node = do
    products <- State.get
    let
        productsCount = CAI.count products
        gridHeight = productsCount / 8
        gridWidth = productsCount / gridHeight
        (rootButtonKey :: ButtonKey) = NK.first -- FIXME, find the next one from state or as passed to the node
        buttonKey = NK.append nodeBoxKey rootButtonKey
        foldF :: Array (Int /\ NestedButtonKey /\ CAI.Product) /\ NestedButtonKey -> (Int /\ CAI.Product) -> Array (Int /\ NestedButtonKey /\ CAI.Product) /\ NestedButtonKey
        foldF (arr /\ prevKey) (index /\ product) =
            let nextKey = NK.next prevKey
            in ((index /\ nextKey /\ product) : arr) /\ nextKey
        createButton :: (Int /\ NestedButtonKey /\ CAI.Product) -> _
        createButton (index /\ key /\ product) =
            B.button key
                [ Box.top $ Offset.px $ 1 + (index `div` gridHeight)
                , Box.left $ Offset.px $ (index `mod` gridWidth) * 3
                , Box.width $ Dimension.px 3
                , Box.height $ Dimension.px 1
                , Style.inputBox
                , Button.mouse true
                , Box.content $ show index
                {-
                , Core.on TextArea.Submit
                    \_ _ -> do
                        content <- TextArea.value ~< textBoxKey
                        let mbNumber = Number.fromString content
                        -- liftEffect $ Console.log content
                        Blessed.lift $ case mbNumber of
                            Just number -> Node.sendInM node FProductPalette._in_product $ T.Number number
                                -- Just number -> Node.sendOut node FNumber._out_out $ T.Number number
                            Nothing -> pure unit
                        pure unit
                 -}
                ]
                [  ]
    _ <- CAI.toArray products
            # mapWithIndex ((/\))
            # foldl foldF ([] /\ buttonKey)
            # Tuple.fst # map createButton
            # traverse \theButton -> nodeBoxKey >~ Node.append theButton
    pure unit
    -- nodeBoxKey >~ Node.append theButton



size :: forall m. FProductPalette.Node m -> Maybe { width :: Int, height :: Int }
size _ = Just { width : 8 * 3 + 3, height : 11 } -- FIXME: we need Effect  to get the proper size from the State


-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit