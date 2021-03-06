module Simulation.Constants where

import Simulation.Types (Constants)

constants :: Constants
constants =
  { eccentricity: 0.7
  , gravity: 12.5
  , massRatio: 1.0
  , timeStep: 0.005
  }
