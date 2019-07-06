module Rpd.API.CommandSequence where

import Prelude ((<<<))

import Data.Array (snoc)

import Rpd.API.Command
import Rpd.Path as Path


data CmdList d c n = CmdList (Array (Command d c n))


infixl 1 andThen as </>


andThen :: forall d c n. CmdList d c n -> Command d c n -> CmdList d c n
andThen (CmdList arr) msg = CmdList (arr `snoc` msg)


init :: forall d c n. CmdList d c n
init = CmdList []


addPatch :: forall d c n. Path.Alias -> Command d c n
addPatch = Request <<< ToAddPatch


-- runSequence
--     :: forall d c n
--      . (String -> Effect Unit)
--     -> CmdList d c n
--     -> Effect Unit
-- runSequence sub (CmdList msgList) = do
--     { event : messages, push : pushMsg } <- Event.create
--     views <-
--         runMUV
--             { event : messages, push : pushMsg }
--             (pure "")
--             { update, view, performEffect }
--     cancel <- Event.subscribe views sub
--     _ <- traverse_ pushMsg msgList
--     --pushMsg Start
--     _ <- cancel
--     pure unit
