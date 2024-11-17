module Cli.App where

import Prelude

import Data.Maybe (fromMaybe, Maybe(..))
import Data.Either (Either(..))
import Data.Map as Map
import Data.String (length, joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (take) as Array
import Data.Foldable (fold)
import Data.Traversable (traverse_)

import Type.Proxy (Proxy)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Aff (runAff_)
import Effect.Console (log) as Console

import Control.Monad.State (modify_, get) as State

import Node.HTTP (Request)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, stat) as Sync
import Node.FS.Aff (readTextFile, stat) as Async

import Cli.State (State)
-- import Cli.State (initial, registerWsClient, connectionsCount, informWsListening, informWsInitialized, withCurrentPatch) as State
-- import Cli.WsServer as WSS
import Cli.Keys (mainScreen, wsStatusButton)
-- import Cli.Ndf.Apply (apply) as NdfFile

-- import Cli.Components.MainScreen as MainScreen
-- import Cli.Components.WsStatusButton as WsButton

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp as Blessed
import Blessed.Internal.Core (Blessed)
import Blessed ((>~))
import Blessed (run, runAnd) as Blessed
import Blessed.UI.Base.Screen.Method as Screen

import Web.Socket.Server as WSS

import Cli.WsServer as WSS

-- import Noodle.Stateful (set) as Stateful
import Noodle.Text.NdfFile as NdfFile
import Noodle.Text.NdfFile.Parser as NdfFile
import Noodle.Text.NdfFile.Apply as NdfFile

import Parsing (runParser) as P

import Options.Applicative as OA
import Options.Applicative ((<**>))

import Cli.State (State)
import Cli.State (init) as State
import Cli.State (informWsInitialized) as State
import Cli.Components.MainScreen as MainScreen

import Noodle.Id (ToolkitR, toolkitR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit.Families (Families)
import Noodle.Text.NdfFile.Codegen as CG
import Noodle.Text.NdfFile.UnitRepr (options) as UnitRepr
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, Options) as CG


data Options
    = JustRun
    | LoadNetworkFrom String
    | GenerateToolkitFrom String
    | SelectToolkit String


defaultOptions = JustRun :: Options


data App pstate (fs :: Families) repr m
    = App Options (State pstate fs repr m)


run :: forall s fs r m. Proxy s -> Toolkit fs r m -> Effect Unit
run ps toolkit = runWith ps toolkit =<< OA.execParser opts
  where -- FIXME: why it shows `run.js` in the info?
    opts = OA.info (options <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc "Noodle Terminal Interface"
     <> OA.header "noodle - Noodle IDE"
      )


runWith :: forall s fs r m. Proxy s -> Toolkit fs r m -> Options -> Effect Unit
runWith ps toolkit options =
    case options of
        JustRun ->
            runBlessedInterface ps toolkit $ pure unit
        LoadNetworkFrom fromFile ->
            runBlessedInterface ps toolkit $ do
                fileCallback <- Blessed.impair1 applyFile
                liftEffect $ runAff_ fileCallback $ Async.readTextFile UTF8 fromFile
                pure unit
        GenerateToolkitFrom fromFile -> do
            generateToolkit UnitRepr.options (Id.toolkitR "Foo") fromFile
        SelectToolkit fromFile -> do
            pure unit -- FIXME: implement
    where
        applyFile :: Either _ String -> BlessedOp (State s fs r m) Effect
        applyFile (Right fileContents) = do
            case P.runParser fileContents NdfFile.parser of
                Right ndfFile ->
                    pure unit
                    -- FIXME: NdfFile.apply ndfFile
                Left parsingError ->
                    pure unit
        applyFile (Left error) =
            pure unit


runBlessedInterface
    :: forall s fs r m
     . Proxy s
    -> Toolkit fs r m
    -> BlessedOp (State s fs r m) Effect
    -> Effect Unit
runBlessedInterface ps toolkit andThen =
    Blessed.runAnd (State.init ps toolkit) MainScreen.component $ do
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
            --liftEffect $ runAff_ productsCallback CAI.requestProducts
            andThen
    where
        handleStart :: Unit -> BlessedOp _ Effect
        handleStart _ =  do
            -- FIXME: State.modify_ State.informWsListening
            -- FIXME: WsButton.updateStatus $ WsButton.Waiting
            mainScreen >~ Screen.render
        handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> BlessedOp (State s fs r m) Effect
        handleMessage _ _ = pure unit
        handleConnection :: WSS.WebSocketConnection -> Request -> BlessedOp (State s fs r m) Effect
        handleConnection wss _ = do
            -- FIXME: State.modify_ $ State.registerWsClient wss
            state <- State.get
            -- FIXME: WsButton.updateStatus $ WsButton.Connected $ fromMaybe 0 $ State.connectionsCount state
            mainScreen >~ Screen.render
            liftEffect $ WSS.sendMessage wss $ WSS.WebSocketMessage "ACK"
        handleError :: Error -> BlessedOp (State s fs r m) Effect
        handleError _ = pure unit
        {- FIXME
        storeProducts :: Either Error CAI.ProductsRequestResult -> BlessedOp State Effect
        storeProducts (Right (Right productsMap)) =
            State.modify_ $ State.withCurrentPatch $ Stateful.set $ CAI.fromMap productsMap
            -- State.modify_ (_ { products = Just productsMap })
            -- Blessed.lift $ Console.log $ show $ Map.size productsMap
        -}
        storeProducts _ = pure unit


options :: OA.Parser Options
options = ado
    nwFromFile <- OA.strOption $ fold
        [ OA.long "file"
        , OA.short 'f'
        , OA.metavar "NETWORK-SOURCE-NDF"
        , OA.value ""
        , OA.help "The file to load the network structure from given NDF file"
        ]

    genFromFile <- OA.strOption $ fold
        [ OA.long "generate"
        , OA.short 'g'
        , OA.metavar "TOOLKIT-SOURCE-NDF"
        , OA.value ""
        , OA.help "Generate Tookit definition from given NDF file"
        ]

    selectToolkit <- OA.strOption $ fold
        [ OA.long "toolkit"
        , OA.short 't'
        , OA.metavar "TOOLKIT-NAME"
        , OA.value ""
        , OA.help "Name of the toolkit to launch with"
        ]

    in
        if (genFromFile /= "") then GenerateToolkitFrom genFromFile
        else if (nwFromFile /= "") then LoadNetworkFrom nwFromFile
        else if (selectToolkit /= "") then SelectToolkit selectToolkit
        else JustRun


generateToolkit :: forall repr. CG.CodegenRepr repr => CG.Options repr -> Id.ToolkitR -> String -> Effect Unit
generateToolkit options toolkitName sourcePath = do
    hydraToolkitText <- liftEffect $ Sync.readTextFile UTF8 sourcePath -- "./src/Demo/Toolkit/Hydra/hydra.v0.3.ndf"
    let eParsedNdf = P.runParser hydraToolkitText NdfFile.parser
    case eParsedNdf of
        Left error -> Console.log $ show error
        Right parsedNdf ->
            if not $ NdfFile.hasFailedLines parsedNdf then do
                --liftEffect $ Console.log $ show $ NdfFile.loadOrder parsedNdf
                let fileMap = NdfFile.codegen toolkitName options parsedNdf
                traverse_ pure $ (Map.toUnfoldable fileMap :: Array (CG.FilePath /\ CG.FileContent))
            else
            Console.log $ "Failed to parse starting at:\n" <> (String.joinWith "\n" $ show <$> (Array.take 3 $ NdfFile.failedLines parsedNdf))