module Components.App where

import Prelude

import Components.NumberInput (mkNumberInput)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, fragment, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Simulation.Constants (initialConsts)
import Simulation.Main as Simulation
import Simulation.World (initialState)

mkApp :: Effect (ReactComponent {})
mkApp = do
  constsRef <- Ref.new initialConsts
  stateRef <- Ref.new (initialState initialConsts)
  frameRef <- Ref.new Nothing
  numberInput <- mkNumberInput

  component "App" \props -> React.do
    eccentricity /\ setEccentricity <- useState initialConsts.eccentricity
    gravity /\ setGravity <- useState initialConsts.gravity
    massRatio /\ setMassRatio <- useState initialConsts.massRatio
    timeStep /\ setTimeStep <- useState initialConsts.timeStep

    let consts = { eccentricity, gravity, massRatio, timeStep }

    useEffect consts do
      Ref.write consts constsRef
      Ref.write (initialState consts) stateRef
      frame <- Ref.read frameRef

      case frame of
        Nothing -> do
          id <- Simulation.run constsRef stateRef
          Ref.write (Just id) frameRef
        Just _ -> pure unit

      pure mempty

    pure
      $ fragment
          [ R.canvas { id: "canvas" }
          , R.div
              { className: "inputs"
              , children:
                [ element numberInput
                    { id: "eccentricity"
                    , label: "Eccentricity"
                    , max: 1.0
                    , min: 0.0
                    , onChange: setEccentricity
                    , step: 0.01
                    , value: eccentricity
                    }
                , element numberInput
                    { id: "gravity"
                    , label: "Gravity"
                    , max: 100.0
                    , min: 1.0
                    , onChange: setGravity
                    , step: 0.5
                    , value: gravity
                    }
                , element numberInput
                    { id: "massRatio"
                    , label: "Mass ratio"
                    , max: 1.0
                    , min: 0.01
                    , onChange: setMassRatio
                    , step: 0.01
                    , value: massRatio
                    }
                , element numberInput
                    { id: "timeStep"
                    , label: "Timestep"
                    , max: 0.15
                    , min: 0.001
                    , onChange: setTimeStep
                    , step: 0.001
                    , value: timeStep
                    }
                ]
              }
          ]
