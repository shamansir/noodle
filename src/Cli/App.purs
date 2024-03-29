module Cli.App where

import Prelude


import Data.Maybe (fromMaybe, Maybe(..))
import Data.Either (Either(..))
import Data.Map as Map
import Data.Foldable (fold)
import Data.String (length) as String

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Aff (runAff_)
import Effect.Console (log) as Console

import Control.Monad.State (modify_, get) as State

import Node.HTTP (Request)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat)

import Cli.State (State)
import Cli.State (initial, registerWsClient, connectionsCount, informWsListening, informWsInitialized, withCurrentPatch) as State
import Cli.WsServer as WSS
import Cli.Keys (mainScreen, wsStatusButton)
import Cli.Ndf.Apply (apply) as NdfFile

import Cli.Components.MainScreen as MainScreen
import Cli.Components.WsStatusButton as WsButton

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp as Blessed
import Blessed.Internal.Core (Blessed)
import Blessed ((>~))
import Blessed (run, runAnd) as Blessed
import Blessed.UI.Base.Screen.Method as Screen

import Web.Socket.Server as WSS

import Noodle.Stateful (set) as Stateful
import Noodle.Text.NdfFile as NdfFile
import Noodle.Text.NdfFile.Parser as NdfFile
import Noodle.Text.NdfFile.Apply as NdfFile

import Parsing (runParser)

import Options.Applicative as OA
import Options.Applicative ((<**>))

import CompArts.Product as CAI


type Options =
    { fromFile :: String
    }


run :: Effect Unit
run = runWith =<< OA.execParser opts
  where -- FIXME: why it shows `run.js` in the info?
    opts = OA.info (options <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc "Noodle Terminal Interface"
     <> OA.header "noodle - Noodle IDE"
      )


runWith :: Options -> Effect Unit
runWith options =
    Blessed.runAnd State.initial MainScreen.component $ do
        hMsg <- Blessed.impair2 handleMessage
        hCon <- Blessed.impair2 handleConnection
        hErr <- Blessed.impair1 handleError
        hStart <- Blessed.impair1 handleStart
        wss <- liftEffect $ WSS.start
            { handleMessage : hMsg
            , handleConnection : hCon
            , handleError : hErr
            , handleStart : hStart
            }
        State.modify_ $ State.informWsInitialized wss
        mainScreen >~ Screen.render
        productsCallback <- Blessed.impair1 storeProducts
        liftEffect $ runAff_ productsCallback CAI.requestProducts
        if String.length options.fromFile > 0 then do
            fileCallback <- Blessed.impair1 applyFile
            liftEffect $ runAff_ fileCallback $ readTextFile UTF8 options.fromFile
            pure unit
        else pure unit
    where
        applyFile :: Either _ String -> BlessedOp State Effect
        applyFile (Right fileContents) = do
            case runParser fileContents NdfFile.parser of
                Right ndfFile ->
                    NdfFile.apply ndfFile
                Left parsingError ->
                    pure unit
        applyFile (Left error) =
            pure unit
        storeProducts :: Either Error CAI.ProductsRequestResult -> BlessedOp State Effect
        storeProducts (Right (Right productsMap)) =
            State.modify_ $ State.withCurrentPatch $ Stateful.set $ CAI.fromMap productsMap
            -- State.modify_ (_ { products = Just productsMap })
            -- Blessed.lift $ Console.log $ show $ Map.size productsMap
        storeProducts _ = pure unit
        handleStart :: Unit -> BlessedOp State Effect
        handleStart _ =  do
            State.modify_ State.informWsListening
            WsButton.updateStatus $ WsButton.Waiting
            mainScreen >~ Screen.render
        handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> BlessedOp State Effect
        handleMessage _ _ = pure unit
        handleConnection :: WSS.WebSocketConnection -> Request -> BlessedOp State Effect
        handleConnection wss _ = do
            State.modify_ $ State.registerWsClient wss
            state <- State.get
            WsButton.updateStatus $ WsButton.Connected $ fromMaybe 0 $ State.connectionsCount state
            mainScreen >~ Screen.render
            liftEffect $ WSS.sendMessage wss $ WSS.WebSocketMessage "ACK"
        handleError :: Error -> BlessedOp State Effect
        handleError _ = pure unit


options :: OA.Parser Options
options = ado
  fromFile <- OA.strOption $ fold
    [ OA.long "file"
    , OA.short 'f'
    , OA.metavar "FILE"
    , OA.value ""
    , OA.help "The file to load the network structure from"
    ]

   in { fromFile }