module Web.Components.SidePanel.WsServerStatus where

import Prelude

import Data.UniqueHash (UniqueHash)
import Data.Text.Format as T
import Data.Array (foldl) as Array
import Data.Maybe (Maybe(..))

import Web.Components.SidePanel (SidePanel)

import WebSocket.Types (Host, Port) as WS

import Noodle.Text.WsMessage (Message(..), toString) as WS


data Status
    = Off -- Disconnected
    | Waiting
    | Connected (Maybe UniqueHash) { total :: Int }
    | Error


type State =
    { host :: WS.Host
    , port :: WS.Port
    , log :: Array WS.Message
    }


sidePanel :: SidePanel "tree" State Status
sidePanel =
    { title : "ws-server"
    , char : statusChar
    , value : _.log >>> extractStatus
    , next : _.log >>> map toTag >>> pure
    }


statusChar :: Status -> Char
statusChar = case _ of
    Off -> '-'
    Error -> 'x'
    Connected _ { total } -> case total of
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        _ -> '*'
    Waiting -> '~'


extractStatus :: Array WS.Message -> Status
extractStatus = Array.foldl foldF Off
    where
        foldF prevStat = case _ of
            WS.CurrentConnection hash ->
                Connected (Just hash) { total : 1 }
            WS.ConnectionsCount nextCount ->
                case prevStat of
                    Off -> Off
                    Waiting -> Waiting
                    Connected mbHash _ -> Connected mbHash { total : nextCount }
                    Error -> Error
            WS.NewConnection _ -> prevStat
            WS.NdfCommand _ -> prevStat
            WS.HydraScene _ -> prevStat
            WS.Disconnected -> Off
            WS.Waiting -> Waiting
            WS.Unknown -> prevStat


toTag :: WS.Message -> T.Tag
toTag = WS.toString >>> T.s