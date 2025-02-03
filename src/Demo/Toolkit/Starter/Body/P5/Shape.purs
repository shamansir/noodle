module Demo.Toolkit.Starter.Body.P5.Shape where

import Prelude
import Demo.Toolkit.Starter.Repr.ChRepr (ValueRepr, Shape(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Noodle.Fn.Process as Fn
import Noodle.Fn.Process as Noodle
import Noodle.Fn.Shape (I, O)
import Noodle.Fn.Shape as Noodle
import Noodle.Fn.Shape.Temperament (Cold, Hot)
import Noodle.Id as NId
import Noodle.Node as Noodle
import Noodle.Node ((#->), (@->))
import Noodle.Toolkit.Families as Noodle
import Noodle.Toolkit.Family as Family
import Noodle.Toolkit.Family as Noodle
import Blessed.UI.Base.Node.Method (append) as Node
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Data.Tuple.Nested ((/\), type (/\))
import Demo.Toolkit.Starter.Repr.ChRepr as VR
import Cli.Keys (NodeBoxKey)
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Type.Data.Symbol (class IsSymbol)


import StarterTk.P5.Shape (Node, _out_shape)


import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
-- import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.BlessedSubj as Subj

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.Core (on) as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.NodeKey (nk, type (<^>), type (<<>>))
import Blessed.Internal.NodeKey (append) as NK

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Method (toggle) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Element.Method (setFront) as Element
import Cli.Components.SidePanel.Console as CC



body :: NId.Family "shape" -> NodeBoxKey -> Node -> BlessedOp Unit Effect
body _ nodeBox node = do
    -- liftEffect $ Console.log "foo"
    let
        rectButton    = shapeButton "■" 0 (NK.append nodeBox rectKey)    $ node @-> (_out_shape /\ Rect)
        circleButton  = shapeButton "⏺" 1 (NK.append nodeBox circleKey)  $ node @-> (_out_shape /\ Circle)
        crossButton   = shapeButton "⨯" 2 (NK.append nodeBox crossKey)   $ node @-> (_out_shape /\ Cross)
        diamondButton = shapeButton "◇" 3 (NK.append nodeBox diamondKey) $ node @-> (_out_shape /\ Diamond)
    nodeBox >~ Node.append rectButton
    nodeBox >~ Node.append circleButton
    nodeBox >~ Node.append crossButton
    nodeBox >~ Node.append diamondButton
    pure unit


rectKey  = nk :: Subj.Button <^> "rect-button-p5-starter"
circleKey  = nk :: Subj.Button <^> "circle-button-p5-starter"
crossKey   = nk :: Subj.Button <^> "cross-button-p5-starter"
diamondKey = nk :: Subj.Button <^> "diamond-button-p5-starter"


-- type CommandInputKey = Button <^> "command-input"


shapeButton :: forall btnid state. IsSymbol btnid ⇒ String → Int → NodeKey Subj.Button btnid → BlessedOp state Effect → Core.Blessed state
shapeButton symbol xcoord bkey onClick =
    B.button bkey
        [ Box.content symbol
        , Box.top $ Offset.px 1
        , Box.left $ Offset.px $ xcoord * 2
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Box.tags true
        -- , Style.addPatch
        , Core.on Button.Press
            \_ _ -> onClick
        {-
        , Core.on Element.MouseOver
            \_ _ -> do
                liftEffect $ Console.log "over"
        , Core.on Element.MouseOut
            \_ _ -> do
                liftEffect $ Console.log "out"
        -}
        ]
        []