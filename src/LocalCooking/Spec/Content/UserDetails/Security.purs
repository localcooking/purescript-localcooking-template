module LocalCooking.Spec.Content.UserDetails.Security where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Snackbar (SnackbarMessage)
import LocalCooking.Types.Env (Env)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.UUID (genUUID, GENUUID)
import Text.Email.Validate (EmailAddress)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (WRITE, READ)
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue




type State = Unit

initialState :: State
initialState = unit

type Action = Unit


type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . { errorMessageQueue        :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , env                      :: Env
        , email ::
          { signal       :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , emailConfirm ::
          { signal       :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , password ::
          { signal       :: IxSignal (Effects eff) String
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , passwordConfirm ::
          { signal       :: IxSignal (Effects eff) String
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , submit ::
          { queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , disabledSignal :: IxSignal (Effects eff) Boolean
          }
        , pendingSignal            :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) State Unit Action
spec
  { errorMessageQueue
  , env
  , email
  , emailConfirm
  , password
  , passwordConfirm
  , submit
  , pendingSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display1
        , align: Typography.center
        } [R.text "Security"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , Email.email
        { label: R.text "Email"
        , fullWidth: true
        , name: "register-email"
        , id: "register-email"
        , emailSignal: email.signal
        , parentSignal: Nothing
        , updatedQueue: email.updatedQueue
        }
      , Email.email
        { label: R.text "Email Confirm"
        , fullWidth: true
        , name: "register-email-confirm"
        , id: "register-email-confirm"
        , emailSignal: emailConfirm.signal
        , parentSignal: Just email.signal
        , updatedQueue: emailConfirm.updatedQueue
        }
      , Password.password
        { label: R.text "Password"
        , fullWidth: true
        , name: "register-password"
        , id: "register-password"
        , passwordSignal: password.signal
        , parentSignal: Nothing
        , updatedQueue: password.updatedQueue
        , errorQueue: passwordErrorQueue
        }
      , Password.password
        { label: R.text "Password Confirm"
        , fullWidth: true
        , name: "register-password-confirm"
        , id: "register-password-confirm"
        , passwordSignal: passwordConfirm.signal
        , parentSignal: Just password.signal
        , updatedQueue: passwordConfirm.updatedQueue
        , errorQueue: passwordConfirmErrorQueue
        }
      , Submit.submit
        { color: Button.secondary
        , variant: Button.raised
        , size: Button.large
        , style: createStyles {marginTop: "1em"}
        , disabledSignal: submit.disabledSignal
        , triggerQueue: submit.queue
        } [R.text "Submit"]
      , pending
        { pendingSignal
        }
      ]
      where
        passwordErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
        passwordConfirmErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue


security :: forall eff
          . { errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
            , env               :: Env
            }
         -> R.ReactElement
security {errorMessageQueue,env} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { env
            , errorMessageQueue
            , email:
              { signal: emailSignal
              , updatedQueue: emailUpdatedQueue
              }
            , emailConfirm:
              { signal: emailConfirmSignal
              , updatedQueue: emailConfirmUpdatedQueue
              }
            , password:
              { signal: passwordSignal
              , updatedQueue: passwordUpdatedQueue
              }
            , passwordConfirm:
              { signal: passwordConfirmSignal
              , updatedQueue: passwordConfirmUpdatedQueue
              }
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              }
            , pendingSignal
            } )
          initialState
  in  R.createElement (R.createClass reactSpec) unit []
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Left ""
    emailConfirmSignal = unsafePerformEff $ IxSignal.make $ Left ""
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
    pendingSignal = unsafePerformEff (IxSignal.make false)
    emailUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    submitQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    _ = unsafePerformEff $ do
      k <- show <$> genUUID
      let submitValue = do
            mEmail <- IxSignal.get emailSignal
            confirm <- IxSignal.get emailConfirmSignal
            case mEmail of
              Right (Just _) -> do
                p1 <- IxSignal.get passwordSignal
                if p1 == ""
                  then IxSignal.set true submitDisabledSignal
                  else do
                    p2 <- IxSignal.get passwordConfirmSignal
                    IxSignal.set (mEmail /= confirm || p1 /= p2) submitDisabledSignal
              _ -> IxSignal.set true submitDisabledSignal
      IxQueue.onIxQueue emailUpdatedQueue k \_ -> submitValue
      IxQueue.onIxQueue emailConfirmUpdatedQueue k \_ -> submitValue
      IxQueue.onIxQueue passwordUpdatedQueue k \_ -> submitValue
      IxQueue.onIxQueue passwordConfirmUpdatedQueue k \_ -> submitValue
      IxSignal.subscribe (\_ -> submitValue) passwordConfirmSignal
      IxQueue.onIxQueue submitQueue k \_ -> do
        eEmail <- IxSignal.get emailSignal
        case eEmail of
          Left _ -> pure unit
          Right email -> pure unit -- TODO submit user details security change
