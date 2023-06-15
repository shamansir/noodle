module Noodle.Text.NetworkFile.Apply where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Traversable (traverse)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Array as Array

import Noodle.Network2 (Network(..))
import Noodle.Network2 as NW
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch)
import Noodle.Node2 as Node
import Noodle.Id as Id

import Unsafe.Coerce (unsafeCoerce)


import Noodle.Text.NetworkFile.Command (Command)
import Noodle.Text.NetworkFile.Command (Command(..)) as C


-- TODO: use NoodleM

-- applyFile :: forall m gstate (nodes :: Row Type) (instances :: Row Type). MonadEffect m => Array Command -> Network gstate nodes instances -> m (Network gstate nodes instances)
applyFile
    :: forall m gstate (nodes :: Row Type) (instances :: Row Type) repr
     . MonadRec m => MonadEffect m
    => Proxy repr -> Network gstate nodes instances -> Array Command -> m (Network gstate nodes instances)
applyFile prepr nw commands =
    Tuple.fst <$> Array.foldM applyCommand (nw /\ nodesMap) commands
    where
        nodesMap :: Map String (Patch.HoldsNode' gstate instances m)
        nodesMap = Map.empty
        applyCommand nw (C.Header _ _) = pure nw
        applyCommand nw (C.MakeNode _ _ _ _) = pure nw
        applyCommand nw (C.Connect srcId srcOutputIdx dstId dstInputIdx) =
            case (/\) <$> Map.lookup srcId nodesMap <*> Map.lookup dstId nodesMap of
                Just ((srcNode :: (Patch.HoldsNode' gstate instances m)) /\ (dstNode :: (Patch.HoldsNode' gstate instances m))) ->
                    Patch.withNode2'
                         srcNode
                         dstNode
                         (\nodeA nodeB _ _ ->
                                        {- nextPatch' <- liftEffect $ Node.withOutputInNodeMRepr
                                                (lco.outputId :: Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr) -- w/o type given here compiler fails to resolve constraints somehow
                                                (\_ onode outputId -> do
                                                    link <- Node.connectByRepr (Proxy :: _ Hydra.WrapRepr) outputId inputId onode inode
                                                    let nextPatch' = Patch.registerLink link curPatch
                                                    pure nextPatch'
                                        ) -}


                            -- case (/\) <$> Node.findHeldOutputByIndex nodeA srcOutputIdx <*> Node.findHeldInputByIndex nodeB srcInputIdx of
                            --     Just ((outputA :: Node.HoldsOutputInNodeMRepr m repr) /\ (inputB :: Node.HoldsInputInNodeMRepr m repr)) ->
                            let
                                (nodeAOutputs :: Array (Node.HoldsOutputInNodeMRepr m repr)) = Node.orderedOutputsMRepr nodeA
                                (nodeBInputs :: Array (Node.HoldsInputInNodeMRepr m repr)) = Node.orderedInputsMRepr nodeB
                                (maybeFoundOutput :: Maybe (Node.HoldsOutputInNodeMRepr m repr)) = Array.index nodeAOutputs srcOutputIdx
                                (maybeFoundInput :: Maybe (Node.HoldsInputInNodeMRepr m repr)) = Array.index nodeBInputs dstInputIdx
                            in case (/\) <$> maybeFoundOutput <*> maybeFoundInput of
                                Just (holdsOutput /\ holdsInput) ->
                                    Node.withOutputInNodeMRepr
                                        holdsOutput
                                        (\_ onode outputId -> do
                                            Node.withInputInNodeMRepr
                                                holdsInput
                                                (\_ inode inputId -> do
                                                    link <- Node.connectByRepr prepr outputId inputId onode inode
                                                    pure nw
                                                )
                                        )
                                Nothing ->
                                    pure nw
                            -- pure nw
                            {-
                            case (/\) <$> (Node.findHeldOutputByIndex nodeA srcOutputIdx) <*> Node.findHeldInputByIndex nodeB srcInputIdx of
                                Just ((outputA :: Node.HoldsOutputInNodeMRepr m repr) /\ (inputB :: Node.HoldsInputInNodeMRepr m repr)) ->
                                    pure nw
                                Nothing -> pure nw
                            -}
                         )
                    -- Node.withNode2'
                    -- Node.withNode'
                    -- Node.findHeldInputByIndex
                    -- Node.connectByRepr prepr outputId inputId onode inode
                Nothing -> pure nw
            -- pure nw