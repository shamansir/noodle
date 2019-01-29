module RpdTest.Network.CollectData
    ( channels, channelsAfter, channelsAfter'
    , node, nodeAfter, nodeAfter'
    , TraceItem(..)
    ) where

import Prelude

import Data.Array (snoc)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Map as Map
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Time.Duration (Milliseconds)

import Effect (Effect, foreachE)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Aff (Aff, delay)

import Rpd.Util (type (/->))
import Rpd.Path as R
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R
import Rpd.API (Rpd, subscribeChannelsData', subscribeNode') as R
import Rpd.Log as RL


infixl 6 snoc as +>


data TraceItem d
  = InletData R.InletPath d
  | OutletData R.OutletPath d

type TracedFlow d = Array (TraceItem d)

derive instance genericTraceItem :: Generic (TraceItem d) _

instance showTraceItem :: Show d => Show (TraceItem d) where
  show = genericShow

instance eqTraceItem :: Eq d => Eq (TraceItem d) where
  eq = genericEq

-- TODO: collect inlet data
-- TODO: collect outlet data


channels
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> Aff (TracedFlow d)
channels period nw =
  channelsAfter period nw $ pure []


channelsAfter
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> R.Rpd (Array R.Canceler)
  -> Aff (TracedFlow d)
channelsAfter period nw afterF = do
  channelsAfter' period nw addNetwork >>= pure <<< snd
  where
    addNetwork = afterF >>= (\effs -> pure $ nw /\ effs)


channelsAfter'
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> R.Rpd (R.Network d /\ Array R.Canceler)
  -> Aff (R.Network d /\ TracedFlow d)
channelsAfter' period nw afterF =
  collectHelper period nw collector afterF
  where
    collector target = do
      let
        onInletData path {- source -} d = do
          curData <- Ref.read target
          Ref.write (curData +> InletData path d) target
          pure unit
        onOutletData path d = do
          curData <- Ref.read target
          Ref.write (curData +> OutletData path d) target
          pure unit
      cancelers <- R.subscribeChannelsData' onOutletData onInletData nw
      pure $ foldCancelers cancelers


node
  :: forall d
   . (Show d)
  => R.NodePath
  -> Milliseconds
  -> R.Network d
  -> Aff (TracedFlow d)
node nodePath period nw =
  nodeAfter nodePath period nw $ pure []


nodeAfter
  :: forall d
   . (Show d)
  => R.NodePath
  -> Milliseconds
  -> R.Network d
  -> R.Rpd (Array R.Canceler)
  -> Aff (TracedFlow d)
nodeAfter nodePath period nw afterF = do
  nodeAfter' nodePath period nw addNetwork >>= pure <<< snd
  where
    addNetwork = afterF >>= (\effs -> pure $ nw /\ effs)


nodeAfter'
  :: forall d
   . (Show d)
  => R.NodePath
  -> Milliseconds
  -> R.Network d
  -> R.Rpd (R.Network d /\ Array R.Canceler)
  -> Aff (R.Network d /\ TracedFlow d)
nodeAfter' nodePath period nw afterF =
  collectHelper period nw collector afterF
  where
    collector target = RL.extract [] $ do
      let
        onNodeData (inletId /\ d) = do
          let inletPath = R.InletPath nodePath inletId
          curData <- Ref.read target
          Ref.write (curData +> InletData inletPath d) target
          pure unit
      canceler <- R.subscribeNode' nodePath onNodeData nw
      pure [ canceler ]


collectHelper
  :: forall d
   . (Show d)
  => Milliseconds
  -> R.Network d
  -> (Ref.Ref (Array (TraceItem d)) -> Effect (Array R.Canceler))
  -> R.Rpd (R.Network d /\ Array R.Canceler)
  -> Aff (R.Network d /\ TracedFlow d)
collectHelper period nw collector afterF = do
  target /\ cancelers <- liftEffect $ do
    target <- Ref.new []
    cancelers <- collector target
    pure $ target /\ cancelers
  nw' /\ userEffects <- liftEffect $ (RL.extract (nw /\ [])) afterF
  delay period
  flow <- liftEffect $ do
    foreachE cancelers identity
    foreachE userEffects identity
    Ref.read target
  pure $ nw' /\ flow


foldCancelers
  :: (R.OutletPath /-> R.Canceler) /\ (R.InletPath /-> R.Canceler)
  -> Array R.Canceler
foldCancelers (outletsMap /\ inletsMap) =
  List.toUnfoldable $ Map.values outletsMap <> Map.values inletsMap
