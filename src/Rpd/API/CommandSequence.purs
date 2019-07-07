module Rpd.API.CommandSequence where

import Prelude ((<<<), (<$>), unit, Unit, bind, pure)

import Effect (Effect)
import Data.Array (snoc)
import Data.Either
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Traversable (traverse_)

import FRP.Event as Event

import Rpd.Network
import Rpd.API (RpdError)
import Rpd.API.Command
import Rpd.API.CommandApply (Step, apply, performEffect)
import Rpd.Path as Path


data CmdList d c n = CmdList (Array (Command d c n))


infixl 1 andThen as </>


andThen :: forall d c n. CmdList d c n -> Command d c n -> CmdList d c n
andThen (CmdList arr) msg = CmdList (arr `snoc` msg)


init :: forall d c n. CmdList d c n
init = CmdList []


addPatch :: forall d c n. Path.Alias -> Command d c n
addPatch = Request <<< ToAddPatch


run
    :: forall d c n
     . Network d c n
    -> CmdList d c n
    -> (Either RpdError (Network d c n) -> Effect Unit)
    -> Effect Unit
run init (CmdList cmdList) sub = do
    { event : commands, push : pushCmd } <- Event.create
    let
        updates =
            Event.fold
                (\cmd ( prog /\ _ ) ->
                    case prog of
                        Left err -> prog /\ []
                        Right model -> apply cmd model)
                commands
                (pure init /\ [])
        progs = fst <$> updates
    _ <- Event.subscribe updates \(prog /\ effects) ->
        traverse_ (\eff -> performEffect eff pushCmd prog) effects
    cancel <- Event.subscribe progs sub
    _ <- traverse_ pushCmd cmdList
    --pushCmd Start
    _ <- cancel
    pure unit
