module LocalCooking.Spec.Form.Email where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Generic (class Generic, gEq)
import Text.Email.Validate (EmailAddress, emailAddress)
import Text.Email.Validate as Email
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
import Queue.Types (READ, WRITE, allowWriting, allowReading)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue



type State =
  { email :: String
  , rerender :: Unit
  }

initialState :: {initEmail :: String} -> State
initialState {initEmail} =
  { email: initEmail
  , rerender: unit
  }

data Action
  = ChangedEmail String
  | SetEmail EmailState
  | EmailUnfocused
  | ReRender

type Effects eff =
  ( ref :: REF
  | eff)


data EmailState
  = EmailPartial String
  | EmailBad String
  | EmailGood EmailAddress

derive instance genericEmailState :: Generic EmailState

instance eqEmailState :: Eq EmailState where
  eq = gEq


spec :: forall eff
      . { emailSignal  :: IxSignal (Effects eff) EmailState
        , parentSignal :: Maybe (IxSignal (Effects eff) EmailState)
        , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , label        :: R.ReactElement
        , fullWidth    :: Boolean
        , name         :: String
        , id           :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { emailSignal
  , parentSignal
  , updatedQueue
  , label
  , fullWidth
  , name
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedEmail e -> do
        liftEff (IxSignal.set (EmailPartial e) emailSignal)
        void $ T.cotransform _ { email = e }
      SetEmail x -> do
        liftEff (IxSignal.set x emailSignal)
        void $ T.cotransform _ { email = case x of
                                    EmailPartial y -> y
                                    EmailBad y -> y
                                    EmailGood y -> Email.toString y
                               }
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      EmailUnfocused -> do
        liftEff $ case emailAddress state.email of
          Nothing -> IxSignal.set (EmailBad state.email) emailSignal
          Just e -> IxSignal.set (EmailGood e) emailSignal
        performAction ReRender props state
        liftEff $ IxQueue.broadcastIxQueue (allowWriting updatedQueue) unit
      ReRender -> void $ T.cotransform _ { rerender = unit }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , value: Input.valueString state.email
        , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
        , error: case unsafePerformEff (IxSignal.get emailSignal) of
          EmailPartial _ -> false
          EmailBad _ -> true
          EmailGood e -> case parentSignal of
            Nothing -> false
            Just parentSignal' -> case unsafePerformEff (IxSignal.get parentSignal') of
              EmailPartial _ -> true
              EmailBad _ -> true
              EmailGood e2 -> e /= e2
        , name
        , id
        } []
      ]



email :: forall eff
       . { label        :: R.ReactElement
         , fullWidth    :: Boolean
         , name         :: String
         , id           :: String
         , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
         , emailSignal  :: IxSignal (Effects eff) EmailState
         , parentSignal :: Maybe (IxSignal (Effects eff) EmailState) --for confirm
         , setQueue     :: One.Queue (write :: WRITE) (Effects eff) EmailState
         } -> R.ReactElement
email {label,fullWidth,name,id,updatedQueue,emailSignal,parentSignal,setQueue} =
  let init =
        { initEmail: case unsafePerformEff (IxSignal.get emailSignal) of
            EmailPartial e -> e
            EmailBad e -> e
            EmailGood y -> Email.toString y
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { label
            , fullWidth
            , name
            , id
            , updatedQueue
            , emailSignal
            , parentSignal
            } ) (initialState init)
      reactSpec' =
          Queue.whileMountedOne
            (allowReading setQueue)
            (\this x -> unsafeCoerceEff $ dispatcher this $ SetEmail x)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
