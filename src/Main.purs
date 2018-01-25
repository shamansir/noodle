module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Data.Function (apply, applyFlipped)
import Rpd as Rpd
import Signal as S
import Signal.Time as ST
import Signal.Channel as SC
import Signal.Loop as SL

-- Elm-style operators

infixr 0 apply as <|
infixl 1 applyFlipped as |>

-- test stuff

hello :: S.Signal String
hello = (ST.every 1000.0) S.~> show

helloEffect :: forall eff. S.Signal (Eff (console :: C.CONSOLE | eff) Unit)
helloEffect = hello S.~> C.log

main_ :: forall eff. Eff (console :: C.CONSOLE | eff) Unit
main_ = S.runSignal helloEffect

-- main function with a custom patch

data MyNodeType = NumNode | StrNode

data MyInletType = NumInlet | StrInlet

-- main :: forall eff. Eff (console :: C.CONSOLE | eff) Unit
-- main =
    -- let
    --     network = Rpd.addPatch "foo" "bar" (Rpd.init "network")
    --     trySignal = S.merge (S.constant "a") (ST.delay 100.0 (S.constant "b"))
    -- in
    --     S.runSignal (trySignal S.~> C.log)

data MsgWrapper n c a x = MsgWrapper Int (Rpd.NetworkMsg n c a x)

main :: Eff (console :: C.CONSOLE, channel :: SC.CHANNEL) Unit
main = void do
  let
      view
        :: forall n c a x
        . (MsgWrapper n c a x)
        -> SL.Emitter (console :: C.CONSOLE, channel :: SC.CHANNEL) (MsgWrapper n c a x)
      view (MsgWrapper num msg) emit = void do
        C.log $ "Received: " <> show num <> " / " <> show msg
        when (num < 10) $ emit (MsgWrapper (num + 1) (Rpd.CreateNetwork "test") )

  -- The loop reads the most recent value from the "future" signal
  -- and uses the view function to display it and simulate the next event.
  SL.runLoop (MsgWrapper 0 (Rpd.CreateNetwork "a")) \future -> map view future



--         keyPressed :: forall e. Int -> Eff (dom :: DOM | e) (Signal Boolean)

--         frames <- animationFrame
--         leftInputs <- keyPressed leftKeyCode
--         rightInputs <- keyPressed rightKeyCode
--         jumpInputs <- keyPressed jumpKeyCode

--         let inputs = { left: _, right: _, jump: _ } <$> leftInputs <*> rightInputs <*> jumpInputs
--         let game = foldp gameLogic initialState (sampleOn frames inputs)
--         runSignal (render <$> game)

-- initialState :: forall eff. Eff (dom :: DOM | eff) GameState
-- initialState = do
--   marioNode <- getMarioNode
--   pure {
--     mario: {
--       node: marioNode,
--       x: -50.0,
--       y: 0.0,
--       dx: 3.0,
--       dy: 6.0,
--       dir: Right
--     }
--   }


-- gameLogic :: forall eff. { left :: Boolean, right :: Boolean, jump :: Boolean } -> Eff (dom :: DOM | eff) GameState -> Eff (dom :: DOM | eff) GameState
-- gameLogic inputs gameState = do
--   gs <- gameState
--   pure (gs { mario = marioLogic inputs gs.mario })


-- render :: forall eff. Eff (dom :: DOM | eff) GameState -> Eff (dom :: DOM | eff) Unit
-- render gameState = do
--   gs <- gameState
--   updatePosition gs.mario
--   updateSprite gs.mario


    -- let
        -- patch = createPatch' "foo"
        -- node = createNode' "num" NumNode
        -- inlet = createInlet' "foo" StrInlet
        -- nodeWithInlet = addInlet' inlet node
        -- (Patch _ sumSignal) = addNode' nodeWithInlet patch
        -- signalLog = S.runSignal ((stringRenderer patch) S.~> log)
    -- in
        -- S.runSignal helloEffect
