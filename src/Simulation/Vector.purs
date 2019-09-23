module Simulation.Vector where

import Prelude
import Simulation.Types (Vector)

vmap :: (Number -> Number) -> Vector -> Vector
vmap f { x, y } = { x: f x, y: f y }

vmul :: Vector -> Number -> Vector
vmul { x, y } m = { x: m * x, y: m * y }
