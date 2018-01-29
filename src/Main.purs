module Main where

import Prelude

import Control.Coroutine (($$), ($~), (~$), (/\))
import Control.Coroutine as Co
import Control.Monad.Aff (Aff, delay, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as C
import Control.Monad.Eff.Exception as E
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Array ((:), head, tail)
import Data.Function (apply, applyFlipped)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.String (joinWith)
import Rpd as Rpd
import Signal as S
import Signal.Channel as SC
import Signal.Loop as SL
import Signal.Time as ST

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

-- main :: Eff (console :: C.CONSOLE, channel :: SC.CHANNEL) Unit
-- main = void do
--   let
--       view
--         :: forall n c a x
--          . Array (Rpd.NetworkMsg n c a x)
--         -> SL.Emitter (console :: C.CONSOLE, channel :: SC.CHANNEL) (Array (Rpd.NetworkMsg n c a x))
--       view msgStack emit = void do
--         let
--             nextMsg = fromMaybe Rpd.Stop (head msgStack)
--             isStop = case nextMsg of
--                 Rpd.Stop -> true
--                 _ -> false
--         C.log $ "Stack: " <> (joinWith " <> " (map show msgStack))
--         C.log $ "Received: " <> show nextMsg
--         when (not isStop) $ emit (fromMaybe [] (tail msgStack))

--   -- The loop reads the most recent value from the "future" signal
--   -- and uses the view function to display it and simulate the next event.
--   SL.runLoop [ Rpd.CreateNetwork "a", Rpd.Stop ] \future -> map view future


-- data Emit a = Emit o a

-- type Producer = Free Emit

-- emit :: o -> Generator Unit
-- emit o = liftF (Emit o unit)

-- emit3 :: Producer Int
-- emit3 = do
--   emit 1
--   emit 2
--   emit 3


-- nats :: forall eff. Co.Producer Int (Aff eff) Unit
-- nats = go 0
--   where
--   go i = do
--     Co.emit i
--     lift (delay (wrap 10.0)) -- 10ms delay
--     go (i + 1)

-- printer :: forall eff. Co.Consumer String (Aff (console :: C.CONSOLE | eff)) Unit
-- printer = forever do
--   s <- Co.await
--   lift (liftEff (C.log s))
--   pure Nothing

-- showing :: forall a m. Show a => Monad m => Co.Transformer a String m Unit
-- showing = forever (Co.transform show)

-- coshowing :: forall eff. Co.CoTransformer String Int (Aff (console :: C.CONSOLE | eff)) Unit
-- coshowing = go 0
--   where
--   go i = do
--     o <- Co.cotransform i
--     lift (liftEff (C.log o))
--     go (i + 1)

-- main :: forall eff. Eff (exception :: E.EXCEPTION, console :: C.CONSOLE | eff) Unit
-- main = void $ launchAff do
--   Co.runProcess (showing `Co.fuseCoTransform` coshowing)
--   Co.runProcess ((nats $~ showing) $$ printer)
--   Co.runProcess (nats /\ nats $$ showing ~$ printer)


main :: Eff (console :: C.CONSOLE, channel :: SC.CHANNEL) Unit
main = void do
    c <- SC.channel (Rpd.CreateNetwork "a")
    let s = SC.subscribe c
    S.runSignal (map show s S.~> C.log)
    map (SC.send c)
        ( Rpd.createNetwork "b"
        : Rpd.addPatch "test" "Test"
        : Rpd.changePatch
            "test"
            ( Rpd.addNode NumNode "aaa" "test"
            : []
            )
        : [])
    -- SC.send c (Rpd.CreateNetwork "b")
    -- SC.send c (Rpd.CreateNetwork "c")
    -- -- Rpd.createNetwork |>
    --     Rpd.addPatch "a" "Test"
    pure s


-- main :: Eff (console :: C.CONSOLE, channel :: SC.CHANNEL) Unit
-- main = void do
--   let view :: Int -> SL.Emitter (console :: C.CONSOLE, channel :: SC.CHANNEL) Int
--       view n emit = void do
--         C.log $ "Received: " <> show n
--         when (n < 500) $ emit (n + 1)

--   -- The loop reads the most recent value from the "future" signal
--   -- and uses the view function to display it and simulate the next event.
--   SL.runLoop 0 \future -> map view future

-- runLoop
--   :: forall eff a
--    . a
--   -> Loop (channel :: CHANNEL | eff) a
--   -> Eff (channel :: CHANNEL | eff) (Signal a)
-- runLoop startVal loopF = do
--   channel' <- channel startVal
--   let signal' = subscribe channel'
--   runSignal (map (_ $ send channel') (loopF signal'))
--   pure signal'



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
