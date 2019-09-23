module Simulation.Main where

import Prelude

import Data.Int (toNumber)
import Effect (Effect, foreachE)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, Dimensions, arc, beginPath, clearRect, fill, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Math (pi)
import Partial.Unsafe (unsafePartial)
import Simulation.Config (config)
import Simulation.Types (Constants, State, Vector)
import Simulation.Utils (getCanvas, scaleCanvas, translatePos)
import Simulation.World (updateState)
import Web.HTML (Window, window)
import Web.HTML.Window (RequestAnimationFrameId, innerHeight, innerWidth, requestAnimationFrame)

run :: Ref Constants -> Ref State -> Effect RequestAnimationFrameId
run constsRef stateRef = do
  canvas <- unsafePartial getCanvas
  ctx <- getContext2D canvas
  wind <- window
  width <- innerWidth wind
  height <- innerHeight wind

  let dims = { width: toNumber width, height: toNumber height }

  scaleCanvas dims wind canvas ctx config.pixelRatio
  requestAnimationFrame (simulate dims wind ctx constsRef stateRef) wind

simulate :: Dimensions -> Window -> Context2D -> Ref Constants -> Ref State -> Effect Unit
simulate dims wind ctx constsRef stateRef = do
  consts <- Ref.read constsRef
  state <- Ref.read stateRef

  let newState = updateState consts state
  Ref.write newState stateRef

  render dims ctx newState
  void $ requestAnimationFrame (simulate dims wind ctx constsRef stateRef) wind

render :: Dimensions -> Context2D -> State -> Effect Unit
render dims ctx state = do
  let
    { width, height } = dims
    { positions } = state
    { bodyColor: color, bodyRadius: radius, scale } = config
    origin = translatePos dims scale { x: 0.0, y: 0.0 }
    start = 0.0
    end = 2.0 * pi

  clearRect ctx { width, height, x: 0.0, y: 0.0 }
  beginPath ctx
  renderOrigin ctx origin

  foreachE (positions <#> translatePos dims scale)
    \{ x, y } -> do
      moveTo ctx (x + radius) y
      arc ctx { x, y, radius, start, end }

  setFillStyle ctx color
  setStrokeStyle ctx color
  fill ctx
  stroke ctx

renderOrigin :: Context2D -> Vector -> Effect Unit
renderOrigin ctx origin = do
  moveTo ctx (origin.x - 4.0) origin.y
  lineTo ctx (origin.x + 4.0) origin.y
  moveTo ctx origin.x (origin.y - 4.0)
  lineTo ctx origin.x (origin.y + 4.0)
