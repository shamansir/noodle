module Noodle.Text.NdfFile.Apply where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Control.Monad.Rec.Class (class MonadRec)
import Prim.RowList as RL

import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)

import Record as Record
import Record.Extra as Record

import Data.Traversable (traverse)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.SProxy (reflect, reflect')
import Data.Array as Array
import Data.List as List
import Data.String.Read (class Read, read)
import Data.Repr (Repr, wrap, fromRepr, class ToRepr, class FromRepr, class ReadWriteRepr, readRepr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Either (Either(..))

import Noodle.Network (Network(..))
import Noodle.Network as NW
import Noodle.Patch as Patch
import Noodle.Patch (Patch)
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Id as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Wiring (class Wiring)

import Unsafe.Coerce (unsafeCoerce)


import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (extractCommands) as NdfFile
import Noodle.Text.NdfFile.Command (Command)
import Noodle.Text.NdfFile.Command (Command(..), NodeFamily(..), NodeId(..), Coord(..), InputId(..), OutputId(..), EncodedValue(..)) as C


type Handlers x gstate instances m repr =
    { onNodeCreated :: Int /\ Int -> Patch.HoldsNode' gstate instances m -> m Unit
    , onNodeCreated2 :: forall f. IsSymbol f => Int /\ Int -> Node.HoldsNode' f m -> m Unit
    , onNodeCreated3:: Int /\ Int -> Patch.HoldsNodeMRepr x gstate instances m repr -> m Unit
    , onConnect :: forall fA fB oA iB. { onode :: Id.NodeIdR, inode :: Id.NodeIdR, outputIndex :: Int, inputIndex :: Int } -> Id.LinkId -> Node.Link fA fB oA iB -> m Unit
    , onConnect2 :: forall fA fB oA iB. { onode :: Id.NodeIdR, inode :: Id.NodeIdR, output :: Id.Output oA, input :: Id.Input iB, outputIndex :: Int, inputIndex :: Int } -> Id.LinkId -> Node.Link fA fB oA iB -> m Unit
    }


type IdMapping x gstate instances m repr = Map String (Patch.HoldsNodeMRepr x gstate instances m repr)


-- TODO: use NoodleM

-- applyFile :: forall m gstate (nodes :: Row Type) (instances :: Row Type). MonadEffect m => Array Command -> Network gstate nodes instances -> m (Network gstate nodes instances)
applyFile
    :: forall x m gstate (families :: Row Type) (instances :: Row Type) repr fsrl isrl
     . Wiring m
    => Id.ListsFamilies families fsrl
    => RL.RowToList instances isrl
    => Record.Keys isrl
    => Toolkit.WithFamilyFn x m gstate families instances repr
    -> Proxy repr
    -> Patch gstate instances
    -> Network gstate families instances
    -> Handlers x gstate instances m repr
    -> NdfFile
    -> m (Network gstate families instances)
applyFile withFamilyFn prepr curPatch nw handlers ndfFile =
    Tuple.fst <$> Array.foldM applyCommand (nw /\ Map.empty) (NdfFile.extractCommands ndfFile)
    where
        -- nodesMap :: IdMapping gstate instances m repr
        -- nodesMap = Map.empty
        tryReadAndSend :: forall f state i din is is' os. Id.HasInput i din is' is => ReadWriteRepr repr => ToRepr din repr => FromRepr repr din => String -> Proxy din -> Node f state is os repr m -> Id.Input i -> m Unit
        tryReadAndSend valueStr _ node input =
            let (maybeDin :: Maybe din) = (readRepr valueStr :: Maybe (Repr repr)) >>= fromRepr
            in case maybeDin of
                Just din -> Node.sendIn node input din *> Node.run node
                Nothing -> pure unit

        tryReadAndSendO :: forall f state o dout is os os'. Id.HasOutput o dout os' os => ReadWriteRepr repr => ToRepr dout repr => FromRepr repr dout => String -> Proxy dout -> Node f state is os repr m -> Id.Output o -> m Unit
        tryReadAndSendO valueStr _ node output =
            let (maybeDout :: Maybe dout) = (readRepr valueStr :: Maybe (Repr repr)) >>= fromRepr
            in case maybeDout of
                Just dout -> Node.sendOut node output dout *> Node.run node
                Nothing -> pure unit
            -- pure unit

        applyCommand :: (Network gstate families instances /\ IdMapping x gstate instances m repr) -> Command -> m (Network gstate families instances /\ IdMapping x gstate instances m repr)

        applyCommand (nw@(Network tk _) /\ nodesMap) (C.Move (C.NodeId nodeId) (C.Coord xPos) (C.Coord yPos)) = do
            pure $ nw /\ nodesMap -- FIXME: TODO

        applyCommand (nw@(Network tk _) /\ nodesMap) (C.Comment _) = do
            pure $ nw /\ nodesMap -- FIXME: TODO

        applyCommand (nw@(Network tk _) /\ nodesMap) (C.MakeNode (C.NodeFamily familyStr) (C.Coord xPos) (C.Coord yPos) (C.NodeId mappingId)) = do
            let nodeFamilies = Toolkit.nodeFamilies (tk :: Toolkit gstate families)
            let maybeFamilyR = List.find (reflect' >>> eq familyStr) nodeFamilies
            case maybeFamilyR of
                Just familyR -> do
                    maybeHeldNode <- withFamilyFn
                        (\family def _ -> do
                            node <- Toolkit.spawn tk family
                            let (nextPatch :: Patch gstate instances) = Patch.registerNode node curPatch
                            let (heldNode :: Patch.HoldsNodeMRepr x gstate instances m repr) = Patch.holdNodeMRepr nextPatch node
                            -- handlers.onNodeCreated  (xPos /\ yPos) (Patch.holdNode' nextPatch node :: Patch.HoldsNode' gstate instances m)
                            -- handlers.onNodeCreated2 (xPos /\ yPos) (Node.holdNode' node)
                            handlers.onNodeCreated3 (xPos /\ yPos) heldNode
                            pure heldNode
                        )
                        familyR
                    pure $ nw /\ fromMaybe nodesMap ((\heldNode -> Map.insert mappingId heldNode nodesMap) <$> maybeHeldNode)
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.Connect (C.NodeId srcId) (C.OutputId (Right srcOutputIdx)) (C.NodeId dstId) (C.InputId (Right dstInputIdx))) =
            case (/\) <$> Map.lookup srcId nodesMap <*> Map.lookup dstId nodesMap of
                Just ((srcNode :: Patch.HoldsNodeMRepr x gstate instances m repr) /\ (dstNode :: Patch.HoldsNodeMRepr x gstate instances m repr)) ->
                    Patch.withNode2MRepr
                         srcNode
                         dstNode
                         (\nodeA nodeB _ _ ->
                            let
                                (nodeAOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedNodeOutputsTest' nodeA
                                (nodeBInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedNodeInputsTest' nodeB
                                (maybeFoundOutput :: Maybe (Node.HoldsOutputInNodeMRepr m repr)) = Array.index nodeAOutputs srcOutputIdx
                                (maybeFoundInput :: Maybe (Node.HoldsInputInNodeMRepr m repr)) = Array.index nodeBInputs dstInputIdx
                            in do
                                case (/\) <$> maybeFoundOutput <*> maybeFoundInput of
                                    Just (holdsOutput /\ holdsInput) ->
                                        Node.withOutputInNodeMRepr
                                            holdsOutput
                                            (\_ onode outputId -> do
                                                Node.withInputInNodeMRepr
                                                    holdsInput
                                                    (\_ inode inputId -> do
                                                        link <- Node.connectByRepr prepr outputId inputId onode inode
                                                        let linkId /\ nextPatch = Patch.registerLink link curPatch
                                                        handlers.onConnect2
                                                            { onode : Id.nodeIdR $ Node.id onode, inode : Id.nodeIdR $ Node.id inode
                                                            , output : outputId, input : inputId
                                                            , outputIndex : srcOutputIdx, inputIndex : dstInputIdx
                                                            }
                                                            linkId
                                                            link
                                                        pure $ nw /\ nodesMap
                                                    )
                                            )
                                    Nothing ->
                                        pure $ nw /\ nodesMap
                         )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.Connect (C.NodeId srcId) (C.OutputId (Left srcOutputId)) (C.NodeId dstId) (C.InputId (Left dstInputId))) =
            case (/\) <$> Map.lookup srcId nodesMap <*> Map.lookup dstId nodesMap of
                Just ((srcNode :: Patch.HoldsNodeMRepr x gstate instances m repr) /\ (dstNode :: Patch.HoldsNodeMRepr x gstate instances m repr)) ->
                    Patch.withNode2MRepr
                         srcNode
                         dstNode
                         (\nodeA nodeB _ _ ->
                            let
                                (nodeAOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedNodeOutputsTest' nodeA
                                (nodeBInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedNodeInputsTest' nodeB
                                (maybeFoundOutput :: Maybe (Int /\ Node.HoldsOutputInNodeMRepr m repr)) = Array.find (\(idx /\ holdsOutput) -> Node.withOutputInNodeMRepr holdsOutput (\_ _ output -> reflect output) == srcOutputId) $ mapWithIndex (/\) nodeAOutputs -- TODO: some typeclass like `HoldsOutput/IsOutput`, to return only Output haha
                                (maybeFoundInput :: Maybe (Int /\ Node.HoldsInputInNodeMRepr m repr)) = Array.find (\(idx /\ holdsInput) -> Node.withInputInNodeMRepr holdsInput (\_ _ input -> reflect input) == dstInputId) $ mapWithIndex (/\) nodeBInputs -- TODO: same
                            in do
                                case (/\) <$> maybeFoundOutput <*> maybeFoundInput of
                                    Just ((oidx /\ holdsOutput) /\ (iidx /\ holdsInput)) ->
                                        Node.withOutputInNodeMRepr
                                            holdsOutput
                                            (\_ onode outputId -> do
                                                Node.withInputInNodeMRepr
                                                    holdsInput
                                                    (\_ inode inputId -> do
                                                        link <- Node.connectByRepr prepr outputId inputId onode inode
                                                        let linkId /\ nextPatch = Patch.registerLink link curPatch
                                                        handlers.onConnect2
                                                            { onode : Id.nodeIdR $ Node.id onode, inode : Id.nodeIdR $ Node.id inode
                                                            , output : outputId, input : inputId
                                                            , outputIndex : oidx, inputIndex : iidx
                                                            }
                                                            linkId
                                                            link
                                                        pure $ nw /\ nodesMap
                                                    )
                                            )
                                    Nothing ->
                                        pure $ nw /\ nodesMap
                         )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.Connect (C.NodeId srcId) (C.OutputId (Right srcOutputIdx)) (C.NodeId dstId) (C.InputId (Left dstInputId))) =
            case (/\) <$> Map.lookup srcId nodesMap <*> Map.lookup dstId nodesMap of
                Just ((srcNode :: Patch.HoldsNodeMRepr x gstate instances m repr) /\ (dstNode :: Patch.HoldsNodeMRepr x gstate instances m repr)) ->
                    Patch.withNode2MRepr
                         srcNode
                         dstNode
                         (\nodeA nodeB _ _ ->
                            let
                                (nodeAOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedNodeOutputsTest' nodeA
                                (nodeBInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedNodeInputsTest' nodeB
                                (maybeFoundOutput :: Maybe (Node.HoldsOutputInNodeMRepr m repr)) = Array.index nodeAOutputs srcOutputIdx
                                (maybeFoundInput :: Maybe (Int /\ Node.HoldsInputInNodeMRepr m repr)) = Array.find (\(idx /\ holdsInput) -> Node.withInputInNodeMRepr holdsInput (\_ _ input -> reflect input) == dstInputId) $ mapWithIndex (/\) nodeBInputs -- TODO: same
                            in do
                                case (/\) <$> maybeFoundOutput <*> maybeFoundInput of
                                    Just (holdsOutput /\ (iidx /\ holdsInput)) ->
                                        Node.withOutputInNodeMRepr
                                            holdsOutput
                                            (\_ onode outputId -> do
                                                Node.withInputInNodeMRepr
                                                    holdsInput
                                                    (\_ inode inputId -> do
                                                        link <- Node.connectByRepr prepr outputId inputId onode inode
                                                        let linkId /\ nextPatch = Patch.registerLink link curPatch
                                                        handlers.onConnect2
                                                            { onode : Id.nodeIdR $ Node.id onode, inode : Id.nodeIdR $ Node.id inode
                                                            , output : outputId, input : inputId
                                                            , outputIndex : srcOutputIdx, inputIndex : iidx
                                                            }
                                                            linkId
                                                            link
                                                        pure $ nw /\ nodesMap
                                                    )
                                            )
                                    Nothing ->
                                        pure $ nw /\ nodesMap
                         )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.Connect (C.NodeId srcId) (C.OutputId (Left srcOutputId)) (C.NodeId dstId) (C.InputId (Right dstInputIdx))) =
            case (/\) <$> Map.lookup srcId nodesMap <*> Map.lookup dstId nodesMap of
                Just ((srcNode :: Patch.HoldsNodeMRepr x gstate instances m repr) /\ (dstNode :: Patch.HoldsNodeMRepr x gstate instances m repr)) ->
                    Patch.withNode2MRepr
                         srcNode
                         dstNode
                         (\nodeA nodeB _ _ ->
                            let
                                (nodeAOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedNodeOutputsTest' nodeA
                                (nodeBInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedNodeInputsTest' nodeB
                                (maybeFoundOutput :: Maybe (Int /\ Node.HoldsOutputInNodeMRepr m repr)) = Array.find (\(idx /\ holdsOutput) -> Node.withOutputInNodeMRepr holdsOutput (\_ _ output -> reflect output) == srcOutputId) $ mapWithIndex (/\) nodeAOutputs -- TODO: some typeclass like `HoldsOutput/IsOutput`, to return only Output haha
                                (maybeFoundInput :: Maybe (Node.HoldsInputInNodeMRepr m repr)) = Array.index nodeBInputs dstInputIdx
                            in do
                                case (/\) <$> maybeFoundOutput <*> maybeFoundInput of
                                    Just ((oidx /\ holdsOutput) /\ holdsInput) ->
                                        Node.withOutputInNodeMRepr
                                            holdsOutput
                                            (\_ onode outputId -> do
                                                Node.withInputInNodeMRepr
                                                    holdsInput
                                                    (\_ inode inputId -> do
                                                        link <- Node.connectByRepr prepr outputId inputId onode inode
                                                        let linkId /\ nextPatch = Patch.registerLink link curPatch
                                                        handlers.onConnect2
                                                            { onode : Id.nodeIdR $ Node.id onode, inode : Id.nodeIdR $ Node.id inode
                                                            , output : outputId, input : inputId
                                                            , outputIndex : oidx, inputIndex : dstInputIdx
                                                            }
                                                            linkId
                                                            link
                                                        pure $ nw /\ nodesMap
                                                    )
                                            )
                                    Nothing ->
                                        pure $ nw /\ nodesMap
                         )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.Send (C.NodeId nodeId) (C.InputId (Right inputIdx)) (C.EncodedValue valueStr)) =
            case Map.lookup nodeId nodesMap of
                Just (nodeHeld :: Patch.HoldsNodeMRepr x gstate instances m repr) ->
                    Patch.withNodeMRepr
                        nodeHeld
                        (\_ node ->
                            let
                                (nodeInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedNodeInputsTest' node
                                (maybeFoundInput :: Maybe (Node.HoldsInputInNodeMRepr m repr)) = Array.index nodeInputs inputIdx
                            in case maybeFoundInput of
                                Just holdsInput -> do
                                    Node.withInputInNodeMRepr
                                        holdsInput
                                        (tryReadAndSend valueStr)
                                    pure $ nw /\ nodesMap
                                Nothing -> pure $ nw /\ nodesMap
                        )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.Send (C.NodeId nodeId) (C.InputId (Left inputId)) (C.EncodedValue valueStr)) =
            case Map.lookup nodeId nodesMap of
                Just (nodeHeld :: Patch.HoldsNodeMRepr x gstate instances m repr) ->
                    Patch.withNodeMRepr
                        nodeHeld
                        (\_ node ->
                            let
                                (nodeInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedNodeInputsTest' node
                                (maybeFoundInput :: Maybe (Node.HoldsInputInNodeMRepr m repr)) = Array.find (\holdsInput -> Node.withInputInNodeMRepr holdsInput (\_ _ input -> reflect input) == inputId) nodeInputs
                            in case maybeFoundInput of
                                Just holdsInput -> do
                                    Node.withInputInNodeMRepr
                                        holdsInput
                                        (tryReadAndSend valueStr)
                                    pure $ nw /\ nodesMap
                                Nothing -> pure $ nw /\ nodesMap
                        )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.SendO (C.NodeId nodeId) (C.OutputId (Right outputIdx)) (C.EncodedValue valueStr)) =
            case Map.lookup nodeId nodesMap of
                Just (nodeHeld :: Patch.HoldsNodeMRepr x gstate instances m repr) ->
                    Patch.withNodeMRepr
                        nodeHeld
                        (\_ node ->
                            let
                                (nodeOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedNodeOutputsTest' node
                                (maybeFoundOutput :: Maybe (Node.HoldsOutputInNodeMRepr m repr)) = Array.index nodeOutputs outputIdx
                            in case maybeFoundOutput of
                                Just holdsOutput -> do
                                    Node.withOutputInNodeMRepr
                                        holdsOutput
                                        (tryReadAndSendO valueStr)
                                    pure $ nw /\ nodesMap
                                Nothing -> pure $ nw /\ nodesMap
                        )
                Nothing -> pure $ nw /\ nodesMap

        applyCommand (nw /\ nodesMap) (C.SendO (C.NodeId nodeId) (C.OutputId (Left outputId)) (C.EncodedValue valueStr)) =
            case Map.lookup nodeId nodesMap of
                Just (nodeHeld :: Patch.HoldsNodeMRepr x gstate instances m repr) ->
                    Patch.withNodeMRepr
                        nodeHeld
                        (\_ node ->
                            let
                                (nodeOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedNodeOutputsTest' node
                                (maybeFoundOutput :: Maybe (Node.HoldsOutputInNodeMRepr m repr)) = Array.find (\holdsOutput -> Node.withOutputInNodeMRepr holdsOutput (\_ _ output -> reflect output) == outputId) nodeOutputs
                            in case maybeFoundOutput of
                                Just holdsOutput -> do
                                    Node.withOutputInNodeMRepr
                                        holdsOutput
                                        (tryReadAndSendO valueStr)
                                    pure $ nw /\ nodesMap
                                Nothing -> pure $ nw /\ nodesMap
                        )
                Nothing -> pure $ nw /\ nodesMap