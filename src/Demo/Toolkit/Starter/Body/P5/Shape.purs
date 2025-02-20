module Demo.Toolkit.Starter.Body.P5.Shape where

import Prelude
import Demo.Toolkit.Starter.Repr.ChRepr (Shape(..))
import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Symbol (class IsSymbol)

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)
-- import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.BlessedSubj as Subj
import Blessed.Internal.NodeKey (nk, type (<^>), type (<<>>))
import Blessed.Internal.NodeKey (append) as NK

import Blessed.UI.Boxes.Box.Option (bg, content, height, left, tags, top, width) as Box
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Node.Method (append) as Node

import Cli.Keys (NodeBoxKey)

import Noodle.Id as NId
import Noodle.Node ((#->), (@->))
import Noodle.Ui.Cli.Palette as Palette

import StarterTk.P5.Shape (Node, _out_shape)



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


rectKey    = nk :: Subj.Button <^> "rect-button-p5-starter"
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
        , Box.bg Palette.nodeBg'
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