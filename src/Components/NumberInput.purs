module Components.NumberInput where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, fragment)

type NumberInput
  = { id :: String
    , label :: String
    , max :: Number
    , min :: Number
    , onChange :: (Number -> Number) -> Effect Unit
    , step :: Number
    , value :: Number
    }

mkNumberInput :: Effect (ReactComponent NumberInput)
mkNumberInput = do
  component "NumberInput" \props -> React.do
    pure
      $ fragment
          [ R.label
              { children: [ R.text props.label ]
              , htmlFor: props.id
              }
          , R.input
              { id: props.id
              , max: show props.max
              , min: show props.min
              , onChange:
                handler targetValue \value ->
                  props.onChange \_ ->
                    (fromMaybe 0.0 <<< fromString <<< fromMaybe "") value
              , step: show props.step
              , type: "number"
              , value: show props.value
              }
          ]
