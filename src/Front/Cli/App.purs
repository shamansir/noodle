module Cli.App where

import Prelude

import Data.Maybe (fromMaybe, Maybe(..))
import Data.Either (Either(..))
import Data.Map as Map
import Data.String (length, joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (take, dropEnd) as Array
import Data.Foldable (fold)
import Data.Traversable (traverse_)
import Data.String (split, Pattern(..)) as String

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Aff (runAff_)
import Effect.Console (log) as Console

import Control.Monad.State (modify_, get) as State

import Node.HTTP (Request)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, stat, exists, mkdir', writeTextFile) as Sync
import Node.FS.Aff (readTextFile, stat) as Async
import Node.FS.Perms (permsReadWrite)

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
import Cli.Components.PaletteTest as PaletteTest

import Noodle.Id (ToolkitR, toolkitR) as Id
import Noodle.Repr (class HasFallback)
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (class HoldsFamilies, class MarkToolkit) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Text.NdfFile.UnitRepr (options) as UnitRepr
import Noodle.Text.NdfFile.Codegen as MCG
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, Options) as FCG

import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At

import Cli.Class.CliFriendly (class CliFriendly)

import Starter.Toolkit (toolkit) as Starter
import Demo.Toolkit.Starter.Repr (options) as Starter


data SelectedToolkit
    -- = Hydra
    = Starter
    -- | Timbre
    | User String


data Options
    = JustRun SelectedToolkit
    | LoadNetworkFrom String SelectedToolkit
    | GenerateToolkitFrom String SelectedToolkit
    | PaletteTest SelectedToolkit


defaultOptions = JustRun defaultToolkit :: Options


defaultToolkit = Starter :: SelectedToolkit


data App (tk :: ToolkitKey) pstate (fs :: Families) repr m
    = App Options (State tk pstate fs repr m)


run :: Effect Unit
run = runWith =<< OA.execParser opts
  where -- FIXME: why it shows `run.js` in the info?
    opts = OA.info (options <**> OA.helper)
     (  OA.fullDesc
     <> OA.progDesc "Noodle Terminal Interface"
     <> OA.header "noodle - Noodle IDE"
     )


runWith :: Options -> Effect Unit
runWith =
    case _ of
        JustRun tkKey ->
            case tkKey of
                Starter -> runBlessedInterface unit Starter.toolkit $ pure unit
                User _  -> pure unit
        LoadNetworkFrom fromFile tkKey ->
            case tkKey of
                Starter -> runBlessedInterface unit Starter.toolkit $ postFix fromFile
                User _  -> pure unit
        GenerateToolkitFrom fromFile tkKey -> do
            case tkKey of
                -- FIXME: even though `Starter` is the actual toolkit name, it mixes up modules names when we want them to be different.
                -- FIXME: Also, it puts families into `Starter.Starter` directory
                -- FIXME: Check the families' modules names as well, since they are `StarterTk` in current configuration...
                -- FIXME: May be put `toolkitName` into `FCG.Options`, and/or generate its module name same way as with families...?
                Starter -> generateToolkit Starter.options (Id.toolkitR "Starter") fromFile
                User _  -> pure unit
        PaletteTest tkKey ->
            runPaletteTest
    where
        postFix fromFile = do
            fileCallback <- Blessed.impair1 applyFile
            liftEffect $ runAff_ fileCallback $ Async.readTextFile UTF8 fromFile
            pure unit
        applyFile :: forall tk s fs r m. Either _ String -> BlessedOp (State tk s fs r m) Effect
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
    :: forall tk s fs repr
     . HasFallback repr
    => Toolkit.HoldsFamilies repr Effect fs
    => CliFriendly tk fs repr Effect
    => s
    -> Toolkit tk fs repr Effect
    -> BlessedOp (State tk s fs repr Effect) Effect
    -> Effect Unit
runBlessedInterface pState toolkit andThen = do
    (initialState :: State tk s fs repr Effect) <- State.init pState toolkit
    Blessed.runAnd initialState (MainScreen.component initialState) $ do
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
        -- productsCallback <- Blessed.impair1 storeProducts
        --liftEffect $ runAff_ productsCallback CAI.requestProducts
        andThen
    where
        handleStart :: Unit -> BlessedOp _ Effect
        handleStart _ =  do
            -- FIXME: State.modify_ State.informWsListening
            -- FIXME: WsButton.updateStatus $ WsButton.Waiting
            mainScreen >~ Screen.render
        handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> BlessedOp (State tk s fs repr Effect) Effect
        handleMessage _ _ = pure unit
        handleConnection :: WSS.WebSocketConnection -> Request -> BlessedOp (State tk s fs repr Effect) Effect
        handleConnection wss _ = do
            -- FIXME: State.modify_ $ State.registerWsClient wss
            state <- State.get
            -- FIXME: WsButton.updateStatus $ WsButton.Connected $ fromMaybe 0 $ State.connectionsCount state
            mainScreen >~ Screen.render
            liftEffect $ WSS.sendMessage wss $ WSS.WebSocketMessage "ACK"
        handleError :: Error -> BlessedOp (State tk s fs repr Effect) Effect
        handleError _ = pure unit
        {- FIXME
        storeProducts :: Either Error CAI.ProductsRequestResult -> BlessedOp State Effect
        storeProducts (Right (Right productsMap)) =
            State.modify_ $ State.withCurrentPatch $ Stateful.set $ CAI.fromMap productsMap
            -- State.modify_ (_ { products = Just productsMap })
            -- Blessed.lift $ Console.log $ show $ Map.size productsMap
        -}


runPaletteTest
    :: Effect Unit
runPaletteTest =
    Blessed.run unit PaletteTest.component


options :: OA.Parser Options
options = ado
    selectToolkit <- OA.strOption $ fold
        [ OA.long "toolkit"
        , OA.short 't'
        , OA.metavar "TOOLKIT-NAME"
        , OA.help "Required: Name of the toolkit to launch with: only `starter` is supported at the time"
        ]

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

    paletteTest <- OA.switch $ fold
        [ OA.long "palette-test"
        , OA.help "Run the palette preview test"
        ]

    let selectedToolkit =
            case selectToolkit of
                "starter" -> Starter
                _ -> defaultToolkit

    in
        if paletteTest then PaletteTest selectedToolkit
        else if (genFromFile /= "") then GenerateToolkitFrom genFromFile selectedToolkit
        else if (nwFromFile /= "") then LoadNetworkFrom nwFromFile selectedToolkit
        else JustRun selectedToolkit


generateToolkit :: forall repr. FCG.CodegenRepr repr => FCG.Options repr -> Id.ToolkitR -> String -> Effect Unit
generateToolkit options toolkitName sourcePath = do
    toolkitText <- liftEffect $ Sync.readTextFile UTF8 sourcePath -- "./src/Demo/Toolkit/Hydra/hydra.v0.3.ndf"
    let eParsedNdf = P.runParser toolkitText NdfFile.parser
    case eParsedNdf of
        Left error -> Console.log $ show error
        Right parsedNdf ->
            if not $ NdfFile.hasFailedLines parsedNdf then do
                --liftEffect $ Console.log $ show $ NdfFile.loadOrder parsedNdf
                let fileMap = NdfFile.codegen toolkitName options parsedNdf
                traverse_ writeCodegenFile $ (Map.toUnfoldable fileMap :: Array (MCG.FilePath /\ MCG.FileContent))
            else
            Console.log $ "Failed to parse starting at:\n" <> (String.joinWith "\n" $ show <$> (Array.take 3 $ NdfFile.failedLines parsedNdf))


writeCodegenFile :: MCG.FilePath /\ MCG.FileContent -> Effect Unit
writeCodegenFile (MCG.FilePath filePath /\ MCG.FileContent fileContent) = do
    let
        outputFilePath = "./src/Demo/Toolkit/Starter/" <> filePath
        outputDirectory = String.joinWith "/" $ Array.dropEnd 1 $ String.split (String.Pattern "/") outputFilePath
    liftEffect $ do
        outputDirectoryExists <- Sync.exists outputDirectory
        when (not outputDirectoryExists) $ Sync.mkdir' outputDirectory { mode : permsReadWrite, recursive : true }
        Sync.writeTextFile UTF8 outputFilePath fileContent