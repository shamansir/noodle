module Cli.App where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.String (joinWith, split, Pattern(..)) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (take, dropEnd) as Array
import Data.Foldable (fold)
import Data.Traversable (traverse_)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import Effect.Console (log) as Console

import Control.Monad.State (modify_, get) as State

import Node.HTTP (Request)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, mkdir', readTextFile, writeTextFile) as Sync
import Node.FS.Perms (permsReadWrite)

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (impair1, impair2) as Blessed
import Blessed.Internal.Core (Blessed)
import Blessed ((>~))
import Blessed (run, runAnd) as Blessed
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.Demo (demo) as BDemo

import Web.Socket.Server (WebSocketConnection, WebSocketMessage(..), sendMessage) as WSS

import Parsing (runParser) as P

import Options.Applicative as OA
import Options.Applicative ((<**>))

import Cli.WsServer (start) as WSS
import Cli.State (State)
import Cli.State (init, informWsInitialized) as CState
import Cli.Components.MainScreen as MainScreen
import Cli.Components.PaletteTest as PaletteTest
import Cli.Class.CliRenderer (ConstantShift)
import Cli.Class.CliFriendly (class CliFriendly)
-- import Cli.State (initial, registerWsClient, connectionsCount, informWsListening, informWsInitialized, withCurrentPatch) as State
-- import Cli.WsServer as WSS
import Cli.Keys (mainScreen, wsStatusButton)
-- import Cli.Ndf.Apply (apply) as NdfFile
import Front.Cli.ApplyNdf (NdfFilePath(..), applyNdfFileFrom)

-- import Cli.Components.MainScreen as MainScreen
-- import Cli.Components.WsStatusButton as WsButton

import Noodle.Id (ToolkitR, toolkitR, FamilyR, toolkit) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (class HoldsFamilies, class FromPatchState) as Toolkit
import Noodle.Toolkit.Families (Families)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Text.NdfFile (codegen, failedLines, hasFailedLines) as NdfFile
import Noodle.Text.NdfFile.Parser (parser)  as NdfFile
import Noodle.Text.NdfFile.Codegen as MCG
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, Options, class ParseableRepr) as FCG

import StarterTk.Toolkit (toolkit) as Starter
import StarterTk.Patch as Starter.Patch
import StarterTk.Repr.GenOptions (genOptions) as Starter

import HydraTk.Toolkit (toolkit) as Hydra
import HydraTk.Patch as Hydra.Patch
import HydraTk.Repr.GenOptions (genOptions) as Hydra


data SelectedToolkit
    = Hydra
    | Starter
    -- | Timbre
    | User String


newtype GenTargetPath = GenTargetPath String


data Options
    = Demo
    | JustRun SelectedToolkit
    | LoadNetworkFrom NdfFilePath SelectedToolkit
    | GenerateToolkitFrom NdfFilePath SelectedToolkit GenTargetPath
    | PaletteTest SelectedToolkit


defaultOptions = JustRun defaultToolkit :: Options


defaultToolkit = Starter :: SelectedToolkit


type Locator = ConstantShift


data App (tk :: ToolkitKey) pstate (fs :: Families) strepr chrepr m
    = App Options (State Locator tk pstate fs strepr chrepr m)


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
                Starter -> runBlessedInterface Starter.Patch.init Starter.toolkit $ pure unit
                Hydra -> runBlessedInterface Starter.Patch.init Starter.toolkit $ pure unit
                User _  -> pure unit -- FIXME
        LoadNetworkFrom ndfFilePath tkKey ->
            case tkKey of
                Starter -> runBlessedInterface Starter.Patch.init Starter.toolkit $ applyNdfFileFrom Starter.toolkit ndfFilePath
                Hydra -> runBlessedInterface Hydra.Patch.init Hydra.toolkit $ applyNdfFileFrom Hydra.toolkit ndfFilePath
                User _  -> pure unit -- FIXME
        GenerateToolkitFrom (NdfFilePath fromFile) tkKey (GenTargetPath genTargetDir) -> do
            case tkKey of
                -- FIXME: even though `Starter` is the actual toolkit name, it mixes up modules names when we want them to be different.
                -- FIXME: Also, it puts families into `Starter.Starter` directory
                -- FIXME: Check the families' modules names as well, since they are `StarterTk` in current configuration...
                -- FIXME: May be put `toolkitName` into `FCG.Options`, and/or generate its module name same way as with families...?
                Starter -> generateToolkit Starter.genOptions (NdfFilePath fromFile) (Id.toolkitR "Starter") (GenTargetPath genTargetDir)
                Hydra   -> generateToolkit Hydra.genOptions   (NdfFilePath fromFile) (Id.toolkitR "Hydra")   (GenTargetPath genTargetDir)
                User _  -> pure unit -- FIXME
        PaletteTest tkKey ->
            runPaletteTest
        Demo ->
            runBlessedDemo


runBlessedInterface
    :: forall tk ps fs strepr chrepr
     . HasFallback chrepr
    => HasFallback strepr
    => FCG.ParseableRepr chrepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromPatchState tk ps strepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => VT.ValueTagged chrepr
    => CliFriendly tk fs chrepr Effect
    => ps
    -> Toolkit tk fs strepr chrepr Effect
    -> BlessedOp (State Locator tk ps fs strepr chrepr Effect) Effect
    -> Effect Unit
runBlessedInterface pState toolkit andThen = do
    (initialState :: State Locator tk ps fs strepr chrepr Effect) <- CState.init pState toolkit
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
        State.modify_ $ CState.informWsInitialized wss
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
        handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> BlessedOp (State Locator tk ps fs strepr chrepr Effect) Effect
        handleMessage _ _ = pure unit
        handleConnection :: WSS.WebSocketConnection -> Request -> BlessedOp (State Locator tk ps fs strepr chrepr Effect) Effect
        handleConnection wss _ = do
            -- FIXME: State.modify_ $ State.registerWsClient wss
            state <- State.get
            -- FIXME: WsButton.updateStatus $ WsButton.Connected $ fromMaybe 0 $ State.connectionsCount state
            mainScreen >~ Screen.render
            liftEffect $ WSS.sendMessage wss $ WSS.WebSocketMessage "ACK"
        handleError :: Error -> BlessedOp (State Locator tk ps fs strepr chrepr Effect) Effect
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


runBlessedDemo :: Effect Unit
runBlessedDemo = BDemo.demo
    -- Blessed.configureJs' BDemo.logEverythingConfig *> BDemo.demo


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

    genTargetPath <- OA.strOption $ fold
        [ OA.long "out"
        , OA.short 'o'
        , OA.metavar "GEN-TARGET-PATH"
        , OA.value defaultGenTargetDir
        , OA.help "Target path for code generation (only works with `-g`)"
        ]

    paletteTest <- OA.switch $ fold
        [ OA.long "palette-test"
        , OA.help "Run the palette preview test"
        ]

    demo <- OA.switch $ fold
        [ OA.long "cli-demo"
        , OA.help "Run the chjj/blessed TUI interface demo"
        ]

    let selectedToolkit =
            case selectToolkit of
                "starter" -> Starter
                "hydra" -> Hydra
                other -> User other

    in
        if demo then Demo
        else if paletteTest then PaletteTest selectedToolkit
        else if (genFromFile /= "") then GenerateToolkitFrom (NdfFilePath genFromFile) selectedToolkit (GenTargetPath genTargetPath)
        else if (nwFromFile /= "")  then LoadNetworkFrom     (NdfFilePath nwFromFile)  selectedToolkit
        else JustRun selectedToolkit


defaultGenTargetDir = "./src/Demo/_Gen" :: String


generateToolkit :: forall strepr chrepr. FCG.CodegenRepr strepr => FCG.CodegenRepr chrepr => FCG.Options strepr chrepr -> NdfFilePath -> Id.ToolkitR -> GenTargetPath -> Effect Unit
generateToolkit genOptions (NdfFilePath sourcePath) toolkitR (GenTargetPath genTargetDir) = do
    toolkitText <- liftEffect $ Sync.readTextFile UTF8 sourcePath
    let eParsedNdf = P.runParser toolkitText NdfFile.parser
    let getToolkitTargetDir = genTargetDir <> "/" <> Id.toolkit toolkitR
    case eParsedNdf of
        Left error -> Console.log $ "Error: " <> show error
        Right parsedNdf ->
            if not $ NdfFile.hasFailedLines parsedNdf then do
                targetDirExists <- Sync.exists getToolkitTargetDir
                when targetDirExists $ throw $ getToolkitTargetDir <> " directory exists, please remove it before generation"
                Console.log $ "Writing to " <> getToolkitTargetDir
                --liftEffect $ Console.log $ show $ NdfFile.loadOrder parsedNdf
                let fileMap = NdfFile.codegen toolkitR genOptions parsedNdf
                traverse_ (writeCodegenFile (GenTargetPath genTargetDir) toolkitR)
                    $ (Map.toUnfoldable fileMap :: Array (MCG.FilePath /\ MCG.FileContent))
            else
            Console.log $ "Failed to parse starting at:\n" <> (String.joinWith "\n" $ show <$> (Array.take 3 $ NdfFile.failedLines parsedNdf))


writeCodegenFile :: GenTargetPath -> Id.ToolkitR -> MCG.FilePath /\ MCG.FileContent -> Effect Unit
writeCodegenFile (GenTargetPath genTargetDir) toolkitR (MCG.FilePath filePath /\ MCG.FileContent fileContent) = do
    let
        outputFilePath = genTargetDir <> "/" <> Id.toolkit toolkitR <> "/" <> filePath
        outputDirectory = String.joinWith "/" $ Array.dropEnd 1 $ String.split (String.Pattern "/") outputFilePath
    liftEffect $ do
        outputDirectoryExists <- Sync.exists outputDirectory
        when (not outputDirectoryExists) $ Sync.mkdir' outputDirectory { mode : permsReadWrite, recursive : true }
        Sync.writeTextFile UTF8 outputFilePath fileContent