module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Math (abs)
import Simulation.Constants (initialConsts)
import Simulation.Types (State)
import Simulation.World (hypotenuse, initialState, updateState)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  let
    { masses, positions } = initialState initialConsts

    initialPos = { x: 1.0, y: 1.0 }
    initialRadius = hypotenuse initialPos
    positiveVel = { x: 1.0, y: 1.0 }
    negativeVel = { x: -1.0, y: -1.0 }

  describe "updateState" do
    it "should bring bodies closer together when velocity is negative" do
      let
        initial :: State
        initial = {
          pos: initialPos,
          vel: negativeVel,
          masses,
          positions
        }
        updated = updateState initialConsts initial
      updated `shouldSatisfy` \{ pos } -> hypotenuse pos < initialRadius

    it "should pull bodies farther apart when velocity is positive" do
      let
        initial :: State
        initial = {
          pos: initialPos,
          vel: positiveVel,
          masses,
          positions
        }
        updated = updateState initialConsts initial
      updated `shouldSatisfy` \{ pos } -> hypotenuse pos > initialRadius

    it "should accelerate when velocity is negative" do
      let
        initial :: State
        initial = {
          pos: initialPos,
          vel: negativeVel,
          masses,
          positions
        }
        updated = updateState initialConsts initial
      updated `shouldSatisfy`
        \{ vel } -> abs vel.x > abs negativeVel.x && abs vel.y > abs negativeVel.y

    it "should decelerate when velocity is positive" do
      let
        initial :: State
        initial = {
          pos: initialPos,
          vel: positiveVel,
          masses,
          positions
        }
        updated = updateState initialConsts initial
      updated `shouldSatisfy`
        \{ vel } -> abs vel.x < abs positiveVel.x && abs vel.y < abs positiveVel.y
