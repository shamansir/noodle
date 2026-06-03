module Test.Spec.RapidInletEdits where

import Prelude

import Data.Array (range)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))

import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Signal ((~>))
import Signal (runSignal) as Signal

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Noodle.Fn.Process (receive, send) as Fn
import Noodle.Node ((#->), (<-@))
import Noodle.Node (_listenUpdatesAndRun, connect, toRaw) as Node
import Noodle.Raw.Node (subscribeChanges) as RawNode

import Example.Toolkit.Minimal.Node.Sum as Sum


spec :: Spec Unit
spec = do

    describe "rapid inlet edits" do

        -- Core regression test.
        -- If _runOnInletUpdates accumulates signal subscriptions (e.g. due to
        -- a second call to runSignal on the same node's inletsCh), the process
        -- will fire more than once per edit. Batches isolate accumulation:
        -- if batch 2 fires more than batch 1, something is growing.
        it "process fires exactly once per inlet edit across batches" $
            liftEffect do
                runCounter <- Ref.new (0 :: Int)
                let
                    countingProcess = do
                        liftEffect $ Ref.modify_ (_ + 1) runCounter
                        a <- Fn.receive Sum.a_in
                        b <- Fn.receive Sum.b_in
                        Fn.send Sum.sum_out $ a + b

                node <- Sum.makeNode' countingProcess
                node # Node._listenUpdatesAndRun
                countBefore <- Ref.read runCounter

                for_ (range 1 5) \i ->
                    node #-> Sum.a_in /\ i
                count5 <- Ref.read runCounter

                for_ (range 6 10) \i ->
                    node #-> Sum.a_in /\ i
                count10 <- Ref.read runCounter

                for_ (range 11 20) \i ->
                    node #-> Sum.a_in /\ i
                count20 <- Ref.read runCounter

                -- each batch must fire exactly (batch_size) times, not growing
                (count5  - countBefore) `shouldEqual` 5
                (count10 - count5)      `shouldEqual` 5
                (count20 - count10)     `shouldEqual` 10

        -- Verifies that the outlet value is the result of the LAST edit, not
        -- some earlier one from a stale queue.
        it "outlet reflects the last sent value after 20 rapid edits" $
            liftEffect do
                node <- Sum.makeNode_ { a: 0, b: 5 } { sum: 0 } Sum.sumBoth
                node # Node._listenUpdatesAndRun

                for_ (range 1 20) \i ->
                    node #-> Sum.a_in /\ i

                result <- node <-@ Sum.sum_out
                -- last a=20, b=5 (unchanged)
                result `shouldEqual` (20 + 5)

        -- subscribeChanges fires on both the inlet update AND the outlet update
        -- produced by running the process. That gives exactly 2 fires per edit.
        -- If the subscription count grows (e.g. a second runSignal on the same
        -- tracker.all), the per-edit count would be 4, 6, 8 … and this fails.
        it "subscribeChanges fires exactly 2 times per inlet edit, not more" $
            liftEffect do
                node <- Sum.makeNode' Sum.sumBoth
                node # Node._listenUpdatesAndRun

                changeCounter <- Ref.new (0 :: Int)
                -- subscribe AFTER _listenUpdatesAndRun so that the initial eager
                -- fire is counted in countBefore, not confused with edit fires
                Signal.runSignal
                    $  RawNode.subscribeChanges (Node.toRaw node)
                    ~> const (Ref.modify_ (_ + 1) changeCounter)
                countBefore <- Ref.read changeCounter

                for_ (range 1 5) \i ->
                    node #-> Sum.a_in /\ i
                count5 <- Ref.read changeCounter

                for_ (range 6 10) \i ->
                    node #-> Sum.a_in /\ i
                count10 <- Ref.read changeCounter

                for_ (range 11 20) \i ->
                    node #-> Sum.a_in /\ i
                count20 <- Ref.read changeCounter

                -- 2 fires per edit, constant across batches
                (count5  - countBefore) `shouldEqual` 10
                (count10 - count5)      `shouldEqual` 10
                (count20 - count10)     `shouldEqual` 20

    -- _runOnInletUpdates is idempotent: calling _listenUpdatesAndRun twice on the
    -- same node must not add a second subscriber. The signal library has no
    -- unsubscribe primitive; the fix is a listenRef guard in _runOnInletUpdates
    -- so only the first call wires up the subscription.
    describe "subscription accumulation: double-init causes extra process fires" do

        it "process still fires exactly once per edit even if _listenUpdatesAndRun is called twice" $
            liftEffect do
                runCounter <- Ref.new (0 :: Int)
                let
                    countingProcess = do
                        liftEffect $ Ref.modify_ (_ + 1) runCounter
                        a <- Fn.receive Sum.a_in
                        b <- Fn.receive Sum.b_in
                        Fn.send Sum.sum_out $ a + b

                node <- Sum.makeNode' countingProcess
                -- double-init: simulates a component that initialises the same node
                -- twice (e.g. re-render / re-mount in the CLI or Web UI)
                node # Node._listenUpdatesAndRun
                node # Node._listenUpdatesAndRun
                countBefore <- Ref.read runCounter

                for_ (range 1 5) \i ->
                    node #-> Sum.a_in /\ i
                count5 <- Ref.read runCounter

                for_ (range 6 10) \i ->
                    node #-> Sum.a_in /\ i
                count10 <- Ref.read runCounter

                -- must still be exactly 1 fire per edit regardless of double-init
                (count5  - countBefore) `shouldEqual` 5
                (count10 - count5)      `shouldEqual` 5

        it "upstream double-init does not cause downstream node to fire twice per edit" $
            liftEffect do
                nodeACounter <- Ref.new (0 :: Int)
                nodeBCounter <- Ref.new (0 :: Int)
                let
                    countingProcessA = do
                        liftEffect $ Ref.modify_ (_ + 1) nodeACounter
                        a <- Fn.receive Sum.a_in
                        b <- Fn.receive Sum.b_in
                        Fn.send Sum.sum_out $ a + b
                    countingProcessB = do
                        liftEffect $ Ref.modify_ (_ + 1) nodeBCounter
                        a <- Fn.receive Sum.a_in
                        b <- Fn.receive Sum.b_in
                        Fn.send Sum.sum_out $ a + b

                nodeA <- Sum.makeNode_ { a: 0, b: 5 } { sum: 0 } countingProcessA
                nodeB <- Sum.makeNode_ { a: 3, b: 0 } { sum: 0 } countingProcessB

                -- double-init on nodeA only; nodeB is normal
                nodeA # Node._listenUpdatesAndRun
                nodeA # Node._listenUpdatesAndRun
                nodeB # Node._listenUpdatesAndRun
                _ <- Node.connect Sum.sum_out Sum.b_in nodeA nodeB

                countABefore <- Ref.read nodeACounter
                countBBefore <- Ref.read nodeBCounter

                for_ (range 1 5) \i ->
                    nodeA #-> Sum.a_in /\ i
                countA5 <- Ref.read nodeACounter
                countB5 <- Ref.read nodeBCounter

                for_ (range 6 10) \i ->
                    nodeA #-> Sum.a_in /\ i
                countA10 <- Ref.read nodeACounter
                countB10 <- Ref.read nodeBCounter

                -- nodeA fires exactly once per edit (idempotency guard);
                -- nodeB must therefore also fire exactly once per edit
                (countB5  - countBBefore) `shouldEqual` 5
                (countB10 - countB5)      `shouldEqual` 5

    describe "rapid inlet edits through a connected chain" do

        -- Simulates Web UI: user creates two nodes, connects upstream outlet to
        -- downstream inlet, then rapidly types into the upstream inlet text field.
        -- Each keystroke should trigger the chain exactly once: upstream fires once,
        -- its outlet propagates to downstream inlet, downstream fires once.
        -- Subscription accumulation bugs would cause counts to grow batch-over-batch.
        it "each node in a two-node chain fires exactly once per inlet edit across batches" $
            liftEffect do
                nodeACounter <- Ref.new (0 :: Int)
                nodeBCounter <- Ref.new (0 :: Int)
                let
                    countingProcessA = do
                        liftEffect $ Ref.modify_ (_ + 1) nodeACounter
                        a <- Fn.receive Sum.a_in
                        b <- Fn.receive Sum.b_in
                        Fn.send Sum.sum_out $ a + b
                    countingProcessB = do
                        liftEffect $ Ref.modify_ (_ + 1) nodeBCounter
                        a <- Fn.receive Sum.a_in
                        b <- Fn.receive Sum.b_in
                        Fn.send Sum.sum_out $ a + b

                nodeA <- Sum.makeNode_ { a: 0, b: 5 } { sum: 0 } countingProcessA
                nodeB <- Sum.makeNode_ { a: 3, b: 0 } { sum: 0 } countingProcessB

                nodeA # Node._listenUpdatesAndRun
                nodeB # Node._listenUpdatesAndRun
                -- connect nodeA.sum_out → nodeB.b_in; initial value is sent to nodeB at this point
                _ <- Node.connect Sum.sum_out Sum.b_in nodeA nodeB

                -- snapshot counters after setup so initial runs don't pollute the delta check
                countABefore <- Ref.read nodeACounter
                countBBefore <- Ref.read nodeBCounter

                for_ (range 1 5) \i ->
                    nodeA #-> Sum.a_in /\ i
                countA5 <- Ref.read nodeACounter
                countB5 <- Ref.read nodeBCounter

                for_ (range 6 10) \i ->
                    nodeA #-> Sum.a_in /\ i
                countA10 <- Ref.read nodeACounter
                countB10 <- Ref.read nodeBCounter

                for_ (range 11 20) \i ->
                    nodeA #-> Sum.a_in /\ i
                countA20 <- Ref.read nodeACounter
                countB20 <- Ref.read nodeBCounter

                -- upstream node: one fire per edit, constant across batches
                (countA5  - countABefore) `shouldEqual` 5
                (countA10 - countA5)      `shouldEqual` 5
                (countA20 - countA10)     `shouldEqual` 10

                -- downstream node: one fire per upstream propagation, same counts
                (countB5  - countBBefore) `shouldEqual` 5
                (countB10 - countB5)      `shouldEqual` 5
                (countB20 - countB10)     `shouldEqual` 10

        -- After 20 rapid edits the downstream outlet must reflect the result of the
        -- LAST edit propagated through the chain, not a stale or intermediate value.
        it "downstream outlet reflects the correct final value after 20 rapid edits through chain" $
            liftEffect do
                -- nodeA: a varies (1..20), b=5 constant → sum = a+5
                -- nodeA.sum_out is wired to nodeB.b_in
                -- nodeB: a=3 constant, b from nodeA.sum_out → sum = 3+(a+5)
                nodeA <- Sum.makeNode_ { a: 0, b: 5 } { sum: 0 } Sum.sumBoth
                nodeB <- Sum.makeNode_ { a: 3, b: 0 } { sum: 0 } Sum.sumBoth

                nodeA # Node._listenUpdatesAndRun
                nodeB # Node._listenUpdatesAndRun
                _ <- Node.connect Sum.sum_out Sum.b_in nodeA nodeB

                for_ (range 1 20) \i ->
                    nodeA #-> Sum.a_in /\ i

                resultA <- nodeA <-@ Sum.sum_out
                resultB <- nodeB <-@ Sum.sum_out

                -- last edit: a_nodeA=20, b_nodeA=5 → sum_nodeA=25
                resultA `shouldEqual` (20 + 5)
                -- nodeB.a=3, nodeB.b=sum_nodeA=25 → sum_nodeB=28
                resultB `shouldEqual` (3 + 20 + 5)
