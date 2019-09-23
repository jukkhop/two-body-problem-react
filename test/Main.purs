module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Math (abs)
import Record (merge)
import Simulation.Constants (constants)
import Simulation.World (hypotenuse, updateState)
import Simulation.World as World
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  let
    initialPos = { x: 1.0, y: 1.0 }
    initialRadius = hypotenuse initialPos
    positiveVel = { x: 1.0, y: 1.0 }
    negativeVel = { x: -1.0, y: -1.0 }
    initialState = merge { pos: initialPos } $ World.initialState constants

  describe "updateState" do
    it "should bring bodies closer together when velocity is negative" do
      let
        initial = merge { vel: negativeVel } initialState
        updated = updateState constants initial
      updated `shouldSatisfy` \{ pos } -> hypotenuse pos < initialRadius

    it "should pull bodies farther apart when velocity is positive" do
      let
        initial = merge { vel: positiveVel } initialState
        updated = updateState constants initial
      updated `shouldSatisfy` \{ pos } -> hypotenuse pos > initialRadius

    it "should accelerate when velocity is negative" do
      let
        initial = merge { vel: negativeVel } initialState
        updated = updateState constants initial
      updated `shouldSatisfy`
        \{ vel } -> abs vel.x > abs negativeVel.x && abs vel.y > abs negativeVel.y

    it "should decelerate when velocity is positive" do
      let
        initial = merge { vel: positiveVel } initialState
        updated = updateState constants initial
      updated `shouldSatisfy`
        \{ vel } -> abs vel.x < abs positiveVel.x && abs vel.y < abs positiveVel.y
