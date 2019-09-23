module Main where

import Prelude

import Components.App (mkApp)
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM (render)
import React.Basic.Hooks (element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  maybeRoot <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  let root = unsafePartial $ fromJust maybeRoot
  app <- mkApp
  render (element app {}) root
