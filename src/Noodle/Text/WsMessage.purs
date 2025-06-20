module Noodle.Text.WsMessage where

import Prelude


import Data.UniqueHash (UniqueHash)
import Data.UniqueHash (unsafeUniqueHash, toString) as UH
import Data.Newtype (unwrap, wrap)
import Data.String (splitAt, take) as String
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Either (Either(..), hush)

import Parsing (ParseError)

import WebSocket.Types (Host, Port, WebSocketMessage, class IsWsMessage)
import Noodle.Text.NdfFile.Command.Op (CommandOp, opToNdf) as Ndf
import Noodle.Text.NdfFile.Parser (parseCommandOp) as Ndf


data Message
    = Waiting
    | CurrentConnection UniqueHash
    | ConnectionsCount Int
    | NewConnection UniqueHash
    | NdfCommand (Maybe Ndf.CommandOp)
    | HydraScene Unit -- FIXME: change to some generic toolkit message, e.g. serialized patch state
    | Disconnected
    | Unknown


fromMessage :: WebSocketMessage -> Message
fromMessage = unwrap >>> decode -- TODO
    where
        decode str =
            let
                { before, after } = String.splitAt 5 str
            in
                case before of
                    "CON::" -> NewConnection     $ UH.unsafeUniqueHash after
                    "CNT::" -> ConnectionsCount  $ fromMaybe (-1) $ Int.fromString after
                    "CUR::" -> CurrentConnection $ UH.unsafeUniqueHash after
                    "NDF::" -> NdfCommand        $ if after == "X" then Nothing else hush $ Ndf.parseCommandOp after
                    "HYD::" -> HydraScene unit
                    "WAI::" -> Waiting
                    "CLS::" -> Disconnected
                    _ -> Unknown


toMessage :: Message -> WebSocketMessage
toMessage = encode >>> wrap
    where
        encode = case _ of
            Waiting ->                  "WAI::"
            NewConnection uhash ->      "CON::" <> UH.toString uhash
            ConnectionsCount count ->   "CNT::" <> show count
            CurrentConnection uhash ->  "CUR::" <> UH.toString uhash
            NdfCommand mbNdfOp ->       "NDF::" <> maybe "X" Ndf.opToNdf mbNdfOp
            HydraScene hydraScene ->    "HYD::"
            Disconnected ->             "CLS::"
            Unknown ->                  "UNK::"


toString :: Message -> String
toString = toMessage >>> unwrap


ndfOp :: Ndf.CommandOp -> Message
ndfOp = Just >>> NdfCommand


instance IsWsMessage Message where
    fromMessage = fromMessage
    toMessage = toMessage


-- (\{- REM)|(-- REM)