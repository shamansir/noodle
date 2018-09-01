module RpdTest.Network.Flow
    ( spec ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Lens ((^.), (?~))
import Data.Lens.At (at)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event.Time (interval)
import Rpd ((</>))
import Rpd as R
import Rpd.Log as RL
import Rpd.Util (type (/->))
import RpdTest.Util (withRpd)
import RpdTest.Network.CollectData (TraceItem(..))
import RpdTest.Network.CollectData as CollectData
import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)


data Delivery
  = Damaged
  | Email
  | Letter
  | Parcel
  | TV
  | IKEAFurniture
  | Car
  | Notebook
  | Curse Int
  | Liver
  | Banana
  | Apple Int
  | Pills

derive instance genericDelivery :: Generic Delivery _

instance showDelivery :: Show Delivery where
  show = genericShow

instance eqDelivery :: Eq Delivery where
  eq = genericEq


type MyRpd = R.Rpd (R.Network Delivery)


-- producingNothingNode :: R.NodeDef Delivery
-- producingNothingNode =
--   { name : "Nothing"
--   , inletDefs : List.Nil
--   , outletDefs : List.Nil
--   , process : R.FlowThrough
--   }


sumCursesToApplesNode :: R.ProcessF Delivery -> R.NodeDef Delivery
sumCursesToApplesNode processF =
  { name : "Sum Curses to Apples"
  , inletDefs
      : curseInlet "curse1"
      : curseInlet "curse2"
      : List.Nil
  , outletDefs
      : appleOutlet "apples"
      : List.Nil
  , process : processF
  }
  where
    curseInlet label =
      { label
      , accept : Just onlyCurses
      , default : Nothing
      }
    appleOutlet label =
      { label
      }
    onlyCurses (Curse _) = true
    onlyCurses _ = false



spec :: Spec Unit
spec = do
  describe "data flow is functioning as expected" $ do

    describe "for inlets"
      inlets

    describe "for outlets"
      outlets

    describe "for links between outlets and nodes"
      links

    describe "for nodes"
      nodes

    describe "for network"
      network


{- ======================================= -}
{- =============== INLETS ================ -}
{- ======================================= -}


inlets :: Spec Unit
inlets = do
  pending "we are able to subscribe some specific inlet in the network"

  pending "we are able to subscribe some specific outlet in the network"

  pending "we are able to subscribe some specific node in the network"

  -- INLETS --

  it "we receive no data from the inlet when it has no flow or default value" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "no-data"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"

    rpd # withRpd \nw -> do
            collectedData <- nw #
              CollectData.channels (Milliseconds 100.0)
            collectedData `shouldEqual` []
            pure unit

    pure unit

  pending "we receive the default value of the inlet just when it was set"

  it "we receive the data sent directly to the inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw
                   #  R.sendToInlet (inletPath 0 0 0) Parcel
                  </> R.sendToInlet (inletPath 0 0 0) Pills
                  </> R.sendToInlet (inletPath 0 0 0) (Curse 5)
            pure []
        collectedData `shouldEqual`
            [ InletData (inletPath 0 0 0) Parcel
            , InletData (inletPath 0 0 0) Pills
            , InletData (inletPath 0 0 0) (Curse 5)
            ]
        pure unit

  it "we receive the values from the data stream attached to the inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel :: R.Canceler <-
              nw # R.streamToInlet
                  (inletPath 0 0 0)
                  (R.flow $ const Pills <$> interval 30)
            pure [ cancel ]
        collectedData `shouldContain`
            (InletData (inletPath 0 0 0) Pills)
        pure unit

    pure unit

  pending "it is possible to manually cancel the streaming-to-inlet procedure"

  it "attaching several simultaneous streams to the inlet allows them to overlap" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            c1 <- nw # R.streamToInlet
                  (inletPath 0 0 0)
                  (R.flow $ const Pills <$> interval 20)
            c2 <- nw # R.streamToInlet
                  (inletPath 0 0 0)
                  (R.flow $ const Banana <$> interval 29)
            pure [ c1, c2 ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) Pills)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) Banana)
        pure unit

    pure unit

  it "when the stream itself was stopped, values are not sent to the inlet anymore" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel <- nw # R.streamToInlet
                (inletPath 0 0 0)
                (R.flow $ const Pills <$> interval 20)
            pure [ cancel ] -- `cancel` is called by `collectDataAfter`
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) Pills)
        collectedData' <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ pure []
        collectedData' `shouldNotContain`
          (InletData (inletPath 0 0 0) Pills)
        pure unit

  it "two different streams may work for different inlets" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet1"
          </> R.addInlet (nodePath 0 0) "inlet2"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            c1 <-
              nw # R.streamToInlet
                (inletPath 0 0 0)
                (R.flow $ const Pills <$> interval 30)
            c2 <-
              nw # R.streamToInlet
                (inletPath 0 0 1)
                (R.flow $ const Banana <$> interval 25)
            pure [ c1, c2 ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) Pills)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) Banana)
        pure unit

    pure unit

  it "same stream may produce values for several inlets" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet1"
          </> R.addInlet (nodePath 0 0) "inlet2"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            let stream = R.flow $ const Banana <$> interval 25
            c1 <- nw # R.streamToInlet (inletPath 0 0 0) stream
            c2 <- nw # R.streamToInlet (inletPath 0 0 1) stream
            pure [ c1, c2 ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) Banana)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) Banana)
        pure unit

    pure unit

  pending "sending data to the inlet triggers the processing function of the node"

  pending "receiving data from the stream triggers the processing function of the node"

  pending "default value of the inlet is sent to its flow when it's added"


{- ======================================= -}
{- =============== OUTLETS =============== -}
{- ======================================= -}


outlets :: Spec Unit
outlets = do
  pending "TODO"


{- ======================================= -}
{- ================ NODES ================ -}
{- ======================================= -}


nodes :: Spec Unit
nodes = do
  pending "adding an inlet inludes its flow into processing"

  it "returning some value from processing function actually sends this value to the outlet (array way)" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode' (patchId 0) (sumCursesToApplesNode (R.IndexBased process))
      process [ Curse a, Curse b ] =
        [ Apple (a + b) ]
      process _ = []

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet (inletPath 0 0 0) (Curse 4)
                    </> R.sendToInlet (inletPath 0 0 1) (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) $ Curse 4)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) $ Curse 3)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) $ Apple 7)
        pure unit

    pure unit

  it "returning some value from processing function actually sends this value to the outlet (labels way)" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode' (patchId 0) (sumCursesToApplesNode (R.LabelBased process))
      processHelper (Curse a) (Curse b) =
        Map.insert "apples" (Apple (a + b)) Map.empty
      processHelper _ _ =
        Map.empty
      process m =
        fromMaybe
          Map.empty
          $ processHelper <$> (m^.at "curse1") <*> (m^.at "curse2")

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet (inletPath 0 0 0) (Curse 4)
                    </> R.sendToInlet (inletPath 0 0 1) (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) $ Curse 4)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) $ Curse 3)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) $ Apple 7)
        pure unit

    pure unit


{- ======================================= -}
{- ================ LINKS ================ -}
{- ======================================= -}


links :: Spec Unit
links = do
  it "connecting some outlet to some inlet makes data flow from this outlet to this inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node1"
          </> R.addOutlet (nodePath 0 0) "outlet"
          </> R.addNode (patchId 0) "node2"
          </> R.addInlet (nodePath 0 1) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel <-
              nw # R.connect (outletPath 0 0 0) (inletPath 0 1 0)
                  </> R.streamToOutlet
                      (outletPath 0 0 0)
                      (R.flow $ const Notebook <$> interval 30)
            pure [ cancel ]
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) Notebook)
        collectedData `shouldContain`
          (InletData (inletPath 0 1 0) Notebook)
        pure unit

    pure unit

  it "connecting some outlet having its own flow to some inlet directs this existing flow to this inlet" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node1"
          </> R.addOutlet (nodePath 0 0) "outlet"
          </> R.addNode (patchId 0) "node2"
          </> R.addInlet (nodePath 0 1) "inlet"

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            cancel <-
              nw # R.streamToOutlet
                (outletPath 0 0 0)
                (R.flow $ const Notebook <$> interval 30)
            _ <- nw # R.connect
                (outletPath 0 0 0)
                (inletPath 0 1 0)
            pure [ cancel ]
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) Notebook)
        collectedData `shouldContain`
          (InletData (inletPath 0 1 0) Notebook)
        pure unit

    pure unit

  it "disconnecting some outlet from some inlet makes the data flow between them stop" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (patchId 0) "node1"
          </> R.addOutlet (nodePath 0 0) "outlet"
          </> R.addNode (patchId 0) "node2"
          </> R.addInlet (nodePath 0 1) "inlet"

    rpd # withRpd \nw -> do
        nw' /\ collectedData <- CollectData.channelsAfter'
          (Milliseconds 100.0)
          nw
          $ do
            -- NB:we're not cancelling this data flow between checks
            _ <- nw # R.streamToOutlet
                  (outletPath 0 0 0)
                  (R.flow $ const Notebook <$> interval 30)
            nw' <- nw # R.connect
                  (outletPath 0 0 0)
                  (inletPath 0 1 0)
            pure $ nw' /\ []
        collectedData `shouldContain`
          (InletData (inletPath 0 1 0) Notebook)
        collectedData' <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw'
          $ do
            _ <-
              nw' # R.disconnectAll (outletPath 0 0 0) (inletPath 0 1 0)
            pure [ ]
        collectedData' `shouldContain`
          (OutletData (outletPath 0 0 0) Notebook)
        collectedData' `shouldNotContain`
          (InletData (inletPath 0 1 0) Notebook)
        pure unit


  pending "default value of the inlet is sent on connection"

  pending "default value for the inlet is sent on disconnection"

    -- describe "processing the output from nodes" do
    --   describe "with predefined function" do
    --     pure unit
    --   describe "with function defined after creation" do
    --     pure unit
    --   describe "after adding an outlet" do
    --     pure unit
    --   describe "after removing an outlet" do
    --     pure unit
    --   describe "after changing the node structure" do
    --     pure unit
    --   describe "after deleting the receiving node" do
    --     pure unit
    --   describe "after adding new node" do
    --     pure unit


{- ======================================= -}
{- =============== NETWORK =============== -}
{- ======================================= -}


network :: Spec Unit
network = do
  it "we receive no data from the network when it's empty" $ do
    (R.init "no-data" :: MyRpd)
      # withRpd \nw -> do
          collectedData <- nw #
            CollectData.channels (Milliseconds 100.0)
          collectedData `shouldEqual` []
          pure unit

    pure unit

  pending "all the cancelers are called after running the system"


patchId :: Int -> R.PatchId
patchId = R.PatchId

nodePath :: Int -> Int -> R.NodePath
nodePath = R.NodePath <<< R.PatchId

inletPath :: Int -> Int -> Int -> R.InletPath
inletPath patchId nodeId inletId = R.InletPath (nodePath patchId nodeId) inletId
-- inletPath = R.InletPath ?_ nodePath

outletPath :: Int -> Int -> Int -> R.OutletPath
outletPath patchId nodeId outletId = R.OutletPath (nodePath patchId nodeId) outletId
-- inletPath = R.InletPath ?_ nodePath


-- logOrExec
--   :: forall a. Either R.RpdError (Effect a) -> Effect a
-- logOrExec effE =
--   either (log <<< show) identity effE
