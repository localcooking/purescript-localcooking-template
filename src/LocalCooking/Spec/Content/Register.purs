module LocalCooking.Spec.Content.Register where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Google.ReCaptcha (reCaptcha)
import LocalCooking.Spec.Snackbar (SnackbarMessage (SnackbarMessageRegister), RegisterError (..))
import LocalCooking.Types.Env (Env)
import Google.ReCaptcha (ReCaptchaResponse)
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues, RegisterInitIn (..), RegisterInitOut (..))
import LocalCooking.Common.Password (hashPassword)
import Facebook.State (FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataRegister))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.UUID (genUUID, GENUUID)
import Text.Email.Validate (EmailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref.Extra (takeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue


type State = Unit

initialState :: State
initialState = unit

data Action
  = SubmitRegister


type Effects eff =
  ( ref       :: REF
  , scrypt    :: SCRYPT
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  | eff)


spec :: forall eff
      . { registerQueues           :: RegisterSparrowClientQueues (Effects eff)
        , errorMessageQueue        :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , toRoot                   :: Eff (Effects eff) Unit
        , env                      :: Env
        , email ::
          { signal        :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          , setValueQueue :: One.Queue (write :: WRITE) (Effects eff) String
          }
        , emailConfirm ::
          { signal        :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          , setValueQueue :: One.Queue (write :: WRITE) (Effects eff) String
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
        , reCaptchaSignal          :: IxSignal (Effects eff) (Maybe ReCaptchaResponse)
        , pendingSignal            :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) State Unit Action
spec
  { registerQueues
  , errorMessageQueue
  , toRoot
  , env
  , reCaptchaSignal
  , pendingSignal
  , email
  , emailConfirm
  , password
  , passwordConfirm
  , submit
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      SubmitRegister -> do
        liftEff $ IxSignal.set true pendingSignal
        mEmail <- liftEff $ IxSignal.get email.signal
        case mEmail of
          Right (Just email) -> do
            mReCaptcha <- liftEff $ IxSignal.get reCaptchaSignal
            case mReCaptcha of
              Just reCaptcha -> do
                passwordString <- liftEff (IxSignal.get password.signal)
                mErr <- liftBase $ do
                  password <- liftBase $ hashPassword
                    { password: passwordString
                    , salt: env.salt
                    }
                  OneIO.callAsync registerQueues $ RegisterInitIn {email,password,reCaptcha}
                liftEff $ do
                  case mErr of
                    Nothing -> pure unit
                    Just initOut -> One.putQueue errorMessageQueue $ case initOut of
                      RegisterInitOutEmailSent ->
                        SnackbarMessageRegister Nothing
                      RegisterInitOutBadCaptcha ->
                        SnackbarMessageRegister $
                          Just RegisterErrorBadCaptchaResponse
                      RegisterInitOutDBError e ->
                        SnackbarMessageRegister $
                          Just RegisterErrorEmailInUse
                  IxSignal.set false pendingSignal
              _ -> pure unit
          _ -> pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display2
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Register"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , grid
        { spacing: Grid.spacing8
        , container: true
        , justify: Grid.centerJustify
        }
        [ grid
          { xs: 6
          , item: true
          }
          [ Email.email
            { label: R.text "Email"
            , fullWidth: true
            , name: "register-email"
            , id: "register-email"
            , emailSignal: email.signal
            , parentSignal: Nothing
            , updatedQueue: email.updatedQueue
            , setValueQueue: Just email.setValueQueue
            }
          , Email.email
            { label: R.text "Email Confirm"
            , fullWidth: true
            , name: "register-email-confirm"
            , id: "register-email-confirm"
            , emailSignal: emailConfirm.signal
            , parentSignal: Just email.signal
            , updatedQueue: emailConfirm.updatedQueue
            , setValueQueue: Just emailConfirm.setValueQueue
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
          , reCaptcha
            { reCaptchaSignal
            , env
            }
          , Submit.submit
            { color: Button.secondary
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {marginTop: "1em"}
            , disabledSignal: submit.disabledSignal
            , triggerQueue: submit.queue
            } [R.text "Submit"]
          ]
        ]
      , pending
        { pendingSignal
        }
      ]
      where
        passwordErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
        passwordConfirmErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue


register :: forall eff
          . { registerQueues       :: RegisterSparrowClientQueues (Effects eff)
            , errorMessageQueue    :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
            , toRoot               :: Eff (Effects eff) Unit
            , env                  :: Env
            , initFormDataRef   :: Ref (Maybe FacebookLoginUnsavedFormData)
            }
         -> R.ReactElement
register
  { registerQueues
  , errorMessageQueue
  , toRoot
  , env
  , initFormDataRef
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { registerQueues
            , errorMessageQueue
            , toRoot
            , env
            , email:
              { signal: emailSignal
              , updatedQueue: emailUpdatedQueue
              , setValueQueue: emailSetValueQueue
              }
            , emailConfirm:
              { signal: emailConfirmSignal
              , updatedQueue: emailConfirmUpdatedQueue
              , setValueQueue: emailConfirmSetValueQueue
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
            , reCaptchaSignal
            , pendingSignal
            } ) initialState
      reactSpec' =
        Queue.whileMountedIxUUID
          submitQueue
          (\this _ -> unsafeCoerceEff $ dispatcher this SubmitRegister)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    emailSignal = unsafePerformEff (IxSignal.make (Left ""))
    emailUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    emailSetValueQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
    emailConfirmSignal = unsafePerformEff (IxSignal.make (Left ""))
    emailConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    emailConfirmSetValueQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    reCaptchaSignal = unsafePerformEff (IxSignal.make Nothing)
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)

    _ = unsafePerformEff $ do
      k <- show <$> genUUID
      let submitValue = do
            mEmail <- IxSignal.get emailSignal
            confirm <- IxSignal.get emailConfirmSignal
            x <- case mEmail of
              Right (Just _) -> do
                p1 <- IxSignal.get passwordSignal
                if p1 == ""
                  then pure true
                  else do
                    p2 <- IxSignal.get passwordConfirmSignal
                    pure (mEmail /= confirm || p1 /= p2)
              _ -> pure true
            IxSignal.set x submitDisabledSignal
      IxQueue.onIxQueue emailUpdatedQueue k \_ -> submitValue
      IxQueue.onIxQueue emailConfirmUpdatedQueue k \_ -> submitValue
      IxQueue.onIxQueue passwordUpdatedQueue k \_ -> submitValue
      IxQueue.onIxQueue passwordConfirmUpdatedQueue k \_ -> submitValue
      IxSignal.subscribe (\_ -> submitValue) emailSignal
      IxSignal.subscribe (\_ -> submitValue) emailConfirmSignal
      IxSignal.subscribe (\_ -> submitValue) passwordSignal
      IxSignal.subscribe (\_ -> submitValue) passwordConfirmSignal

      mX <- takeRef initFormDataRef
      case mX of
        Just x -> do
          unsafeCoerceEff $ log "Taken by register..."
          case x of
            FacebookLoginUnsavedFormDataRegister {email,emailConfirm} -> do
              One.putQueue emailSetValueQueue email
              One.putQueue emailConfirmSetValueQueue emailConfirm
            _ -> pure unit
        _ -> pure unit
