module Test.Spec.RapidInletEdits where

import Prelude

import Data.Array (range)
import Data.Traversable (for_)

import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Signal ((~>))
import Signal (runSignal) as Signal

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Noodle.Fn.Process (receive, send) as Fn
import Noodle.Node ((#->), (<-@))
import Noodle.Node (_listenUpdatesAndRun, toRaw) as Node
import Noodle.Raw.Node (subscribeChanges) as RawNode

import Example.Toolkit.Minimal.Node.Sum as Sum


spec :: Spec Unit
spec =

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
