module LocalCooking.Spec.Content.UserDetails.Security where

import Prelude

import Thermite as T
import React as R
import React.DOM as R

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography




type State = Unit

initialState :: State
initialState = unit

type Action = Unit


spec :: forall eff
      . T.Spec eff State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display1
        , align: Typography.center
        } [R.text "Security"]
      -- , textField
      --   { label: R.text "Email Address"
      --   , onChange: 
      --   }
      ]


security :: R.ReactElement
security =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
