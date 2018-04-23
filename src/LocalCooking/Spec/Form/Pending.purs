module LocalCooking.Spec.Form.Pending where

import Prelude
import Data.UUID (GENUUID)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal

import MaterialUI.CircularProgress (circularProgress)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal


type State =
  { pending :: Boolean
  }

initialState :: {initPending :: Boolean} -> State
initialState {initPending} =
  { pending: initPending
  }

data Action
  = ChangedPending Boolean

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff
      . { pendingSignal :: IxSignal (Effects eff) Boolean
        }
     -> T.Spec (Effects eff) State Unit Action
spec {pendingSignal} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedPending p -> void $ T.cotransform _ { pending = p }

    render :: T.Render State Unit Action
    render dispatch props state children =
      if state.pending
        then [ R.div
                [ RP.style
                  { zIndex: 1000
                  , position: "absolute"
                  , top: "0"
                  , left: "0"
                  , right: "0"
                  , bottom: "0"
                  , display: "flex"
                  , flexDirection: "column"
                  , alignItems: "center"
                  , justifyContent: "center"
                  , background: "rgba(255,255,255, 0.6)"
                  }
                ]
                [ circularProgress {size: 50}
                ]
             ]
        else []


pending :: forall eff
         . { pendingSignal :: IxSignal (Effects eff) Boolean
           } -> R.ReactElement
pending {pendingSignal} =
  let init =
        { initPending: unsafePerformEff (IxSignal.get pendingSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec {pendingSignal}) (initialState init)
      reactSpec' =
           Signal.whileMountedIxUUID
             pendingSignal
             (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPending x))
             reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
