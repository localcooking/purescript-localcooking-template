module LocalCooking.Spec.Form.Password where

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue

import MaterialUI.TextField (textField)
import MaterialUI.Input as Input

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (READ, WRITE)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { password :: String
  , rerender :: Unit
  , error    :: Boolean
  }

initialState :: {initPassword :: String} -> State
initialState {initPassword} =
  { password: initPassword
  , rerender: unit
  , error: false
  }

data Action
  = ChangedPassword String
  | PasswordUnfocused
  | ReRender
  | Error

type Effects eff =
  ( ref :: REF
  | eff)

spec :: forall eff
      . { passwordSignal :: IxSignal (Effects eff) String
        , parentSignal   :: Maybe (IxSignal (Effects eff) String)
        , updatedQueue   :: IxQueue (read :: READ) (Effects eff) Unit
        , label          :: R.ReactElement
        , fullWidth      :: Boolean
        , name           :: String
        , id             :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { passwordSignal
  , parentSignal
  , updatedQueue
  , label
  , fullWidth
  , name
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedPassword e -> do
        liftEff $ IxSignal.set e passwordSignal
        void $ T.cotransform _ { password = e, error = false }
      PasswordUnfocused -> do
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (IxQueue.allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }
      Error -> void $ T.cotransform _ { error = true }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , onChange: mkEffFn1 \e -> dispatch $ ChangedPassword (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch PasswordUnfocused
        , "type": Input.passwordType
        , error:
          let p = unsafePerformEff (IxSignal.get passwordSignal)
              withParent = case parentSignal of
                Nothing -> false
                Just passwordConfirmSignal ->
                  let p2 = unsafePerformEff (IxSignal.get passwordConfirmSignal)
                  in  p /= p2
          in  state.error || withParent
        , name
        , id
        } []
      ]



password :: forall eff
       . { label          :: R.ReactElement
         , fullWidth      :: Boolean
         , name           :: String
         , id             :: String
         , errorQueue     :: One.Queue (write :: WRITE) (Effects eff) Unit
         , updatedQueue   :: IxQueue (read :: READ) (Effects eff) Unit
         , passwordSignal :: IxSignal (Effects eff) String
         , parentSignal   :: Maybe (IxSignal (Effects eff) String) --for confirm
         } -> R.ReactElement
password
  { label
  , fullWidth
  , name
  , id
  , updatedQueue
  , passwordSignal
  , parentSignal
  , errorQueue
  } =
  let init =
        { initPassword: unsafePerformEff (IxSignal.get passwordSignal)
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , name
            , id
            , updatedQueue
            , passwordSignal
            , parentSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (One.allowReading errorQueue)
            (\this _ -> unsafeCoerceEff $ dispatcher this Error)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
