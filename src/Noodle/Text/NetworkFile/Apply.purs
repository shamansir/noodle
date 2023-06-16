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
import Data.SProxy (reflect')
import Data.Array as Array
import Data.List as List

import Noodle.Network2 (Network(..))
import Noodle.Network2 as NW
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Id as Id
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit

import Unsafe.Coerce (unsafeCoerce)


import Noodle.Text.NetworkFile.Command (Command)
import Noodle.Text.NetworkFile.Command (Command(..)) as C


type Handlers gstate instances m =
    { onNodeCreated :: Int /\ Int -> Patch.HoldsNode' gstate instances m -> m Unit
    , onNodeCreated2 :: forall f. IsSymbol f => Int /\ Int -> Node.HoldsNode' f m -> m Unit
    , onConnect :: forall fA fB oA iB. Node.Link fA fB oA iB -> m Unit
    }


-- TODO: use NoodleM

-- applyFile :: forall m gstate (nodes :: Row Type) (instances :: Row Type). MonadEffect m => Array Command -> Network gstate nodes instances -> m (Network gstate nodes instances)
applyFile
    :: forall m gstate (families :: Row Type) (instances :: Row Type) repr fsrl
     . MonadRec m
    => MonadEffect m
    => Id.ListsFamilies families fsrl
    => Toolkit.WithFamilyFn m gstate families instances repr
    -> Proxy repr
    -> Network gstate families instances
    -> Handlers gstate instances m
    -> Array Command
    -> m (Network gstate families instances)
applyFile withFamilyFn prepr nw handlers commands =
    Tuple.fst <$> Array.foldM applyCommand (nw /\ nodesMap) commands
    where
        nodesMap :: Map String (Patch.HoldsNode' gstate instances m)
        nodesMap = Map.empty
        applyCommand :: (Network gstate families instances /\ Map String (Patch.HoldsNode' gstate instances m)) -> Command -> m (Network gstate families instances /\ Map String (Patch.HoldsNode' gstate instances m))
        applyCommand pair (C.Header _ _) = pure pair
        applyCommand pair@((Network tk _) /\ _) (C.MakeNode familyStr xPos yPos _) = do
            let nodeFamilies = Toolkit.nodeFamilies (tk :: Toolkit gstate families)
            let maybeFamilyR = List.find (reflect' >>> eq familyStr) nodeFamilies
            case maybeFamilyR of
                Just familyR -> do
                    _ <- withFamilyFn
                        (\family def _ -> do
                            node <- Toolkit.spawn tk family
                            handlers.onNodeCreated2 (xPos /\ yPos) (Node.holdNode' node)
                            pure unit
                        )
                        familyR
                    pure pair
                Nothing -> pure pair
        applyCommand pair (C.Connect srcId srcOutputIdx dstId dstInputIdx) =
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
                                                    handlers.onConnect link
                                                    pure pair
                                                )
                                        )
                                Nothing ->
                                    pure pair
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
                Nothing -> pure pair
            -- pure nw