module LocalCooking.Spec.Form.Submit where

import Prelude
import Data.UUID (GENUUID)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React as R
import React.DOM as R
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (Styles)
import MaterialUI.Button (button)
import MaterialUI.Button as Button

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (READ)
import IxQueue (IxQueue)
import IxQueue as IxQueue


type State =
  { disabled :: Boolean
  }

initialState :: {initDisabled :: Boolean} -> State
initialState {initDisabled} =
  { disabled: initDisabled
  }

data Action
  = ChangedDisabled Boolean
  | Clicked


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  | eff)

spec :: forall eff
      . { color        :: Button.Color
        , variant      :: Button.Variant
        , size         :: Button.Size
        , style        :: Styles
        , triggerQueue :: IxQueue (read :: READ) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {color,variant,size,style,triggerQueue} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedDisabled d -> void $ T.cotransform _ { disabled = d }
      Clicked -> liftEff (IxQueue.broadcastIxQueue (IxQueue.allowWriting triggerQueue) unit)

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ button
        { color
        , variant
        , size
        , style
        , disabled: state.disabled
        , onTouchTap: mkEffFn1 \_ -> dispatch Clicked
        } children
      ]


submit :: forall eff
       . { color          :: Button.Color
         , variant        :: Button.Variant
         , size           :: Button.Size
         , style          :: Styles
         , triggerQueue   :: IxQueue (read :: READ) (Effects eff) Unit
         , disabledSignal :: IxSignal (Effects eff) Boolean
         } -> Array R.ReactElement -> R.ReactElement
submit {disabledSignal,color,variant,size,style,triggerQueue} =
  let init =
        { initDisabled: unsafePerformEff (IxSignal.get disabledSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec {color,variant,size,style,triggerQueue}) (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            disabledSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedDisabled x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit
