module LocalCooking.Spec.Content.Register where

import LocalCooking.Spec.Content.Register.Pending (pending)
import LocalCooking.Spec.Form.Email (email)
import LocalCooking.Spec.Form.Password (password)
import LocalCooking.Spec.Form.Submit (submit)
import LocalCooking.Spec.Google.ReCaptcha (reCaptcha)
import LocalCooking.Spec.Snackbar (SnackbarMessage (SnackbarMessageRegister), RegisterError (..))
import LocalCooking.Types.Env (Env)
import Google.ReCaptcha (ReCaptchaResponse)
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues, RegisterInitIn (..), RegisterInitOut (..))
import LocalCooking.Common.Password (hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.UUID (genUUID, GENUUID)
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import React.Queue.WhileMounted as Queue

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import MaterialUI.TextField (textField)
import MaterialUI.Input as Input
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.CircularProgress (circularProgress)
import Crypto.Scrypt (SCRYPT)

import Unsafe.Coerce (unsafeCoerce)
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

type Action = Unit


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
        , emailSignal              :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
        , emailConfirmSignal       :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
        , passwordSignal              :: IxSignal (Effects eff) String
        , passwordConfirmSignal       :: IxSignal (Effects eff) String
        , submitDisabledSignal     :: IxSignal (Effects eff) Boolean
        , reCaptchaSignal          :: IxSignal (Effects eff) (Maybe ReCaptchaResponse)
        , pendingSignal            :: IxSignal (Effects eff) Boolean
        , submitQueue              :: IxQueue (read :: READ) (Effects eff) Unit
        , emailUpdatedQueue        :: IxQueue (read :: READ) (Effects eff) Unit
        , emailConfirmUpdatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , passwordUpdatedQueue        :: IxQueue (read :: READ) (Effects eff) Unit
        , passwordConfirmUpdatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
        } -> T.Spec (Effects eff) State Unit Action
spec
  { registerQueues
  , errorMessageQueue
  , toRoot
  , env
  , emailSignal
  , emailConfirmSignal
  , passwordSignal
  , passwordConfirmSignal
  , submitDisabledSignal
  , reCaptchaSignal
  , pendingSignal
  , submitQueue
  , emailUpdatedQueue
  , emailConfirmUpdatedQueue
  , passwordUpdatedQueue
  , passwordConfirmUpdatedQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

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
          [ email
            { label: R.text "Email"
            , fullWidth: true
            , name: "register-email"
            , id: "register-email"
            , emailSignal
            , parentSignal: Nothing
            , updatedQueue: emailUpdatedQueue
            }
          , email
            { label: R.text "Email Confirm"
            , fullWidth: true
            , name: "register-email-confirm"
            , id: "register-email-confirm"
            , emailSignal: emailConfirmSignal
            , parentSignal: Just emailSignal
            , updatedQueue: emailConfirmUpdatedQueue
            }
          , password
            { label: R.text "Password"
            , fullWidth: true
            , name: "register-password"
            , id: "register-password"
            , passwordSignal
            , parentSignal: Nothing
            , updatedQueue: passwordUpdatedQueue
            }
          , password
            { label: R.text "Password Confirm"
            , fullWidth: true
            , name: "register-password-confirm"
            , id: "register-password-confirm"
            , passwordSignal: passwordConfirmSignal
            , parentSignal: Just passwordSignal
            , updatedQueue: passwordConfirmUpdatedQueue
            }
          , reCaptcha
            { reCaptchaSignal
            , env
            }
          , submit
            { color: Button.secondary
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {marginTop: "1em"}
            , disabledSignal: submitDisabledSignal
            , triggerQueue: submitQueue
            } [R.text "Submit"]
          ]
        ]
      , pending
        { pendingSignal
        }
      ]


register :: forall eff
          . { registerQueues       :: RegisterSparrowClientQueues (Effects eff)
            , errorMessageQueue    :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
            , toRoot               :: Eff (Effects eff) Unit
            , env                  :: Env
            }
         -> R.ReactElement
register
  { registerQueues
  , errorMessageQueue
  , toRoot
  , env
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { registerQueues
            , errorMessageQueue
            , toRoot
            , env
            , emailSignal
            , emailConfirmSignal
            , passwordSignal
            , passwordConfirmSignal
            , submitDisabledSignal
            , reCaptchaSignal
            , pendingSignal
            , submitQueue
            , emailUpdatedQueue
            , emailConfirmUpdatedQueue
            , passwordUpdatedQueue
            , passwordConfirmUpdatedQueue
            } ) initialState
  in  R.createElement (R.createClass reactSpec) unit []
  where
    emailSignal = unsafePerformEff (IxSignal.make (Left ""))
    emailConfirmSignal = unsafePerformEff (IxSignal.make (Left ""))
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
    reCaptchaSignal = unsafePerformEff (IxSignal.make Nothing)
    pendingSignal = unsafePerformEff (IxSignal.make false)
    emailUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    submitQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    _ = unsafePerformEff $ do
      k <- show <$> genUUID
      IxQueue.onIxQueue emailUpdatedQueue k \_ -> do
        mEmail <- IxSignal.get emailSignal
        confirm <- IxSignal.get emailConfirmSignal
        case mEmail of
          Right (Just _) -> IxSignal.set (mEmail /= confirm) submitDisabledSignal
          _ -> IxSignal.set true submitDisabledSignal
      IxQueue.onIxQueue emailConfirmUpdatedQueue k \_ -> do
        mEmail <- IxSignal.get emailSignal
        confirm <- IxSignal.get emailConfirmSignal
        case mEmail of
          Right (Just _) -> IxSignal.set (mEmail /= confirm) submitDisabledSignal
          _ -> IxSignal.set true submitDisabledSignal
      IxQueue.onIxQueue submitQueue k \_ -> do
        IxSignal.set true pendingSignal
        mEmail <- IxSignal.get emailSignal
        case mEmail of
          Right (Just email) -> do
            mReCaptcha <- IxSignal.get reCaptchaSignal
            case mReCaptcha of
              Nothing -> pure unit
              Just reCaptcha -> do
                let resolve eMErr = case eMErr of
                      Left e -> throwException e
                      Right mErr -> do
                        IxSignal.set false pendingSignal
                        case mErr of
                          Nothing -> pure unit
                          Just initOut -> One.putQueue errorMessageQueue $ case initOut of
                            RegisterInitOutEmailSent ->
                                SnackbarMessageRegister Nothing
                            RegisterInitOutBadCaptcha ->
                                SnackbarMessageRegister
                              $ Just RegisterErrorBadCaptchaResponse
                            RegisterInitOutDBError e ->
                                SnackbarMessageRegister
                              $ Just RegisterErrorEmailInUse
                runAff_ resolve $ do
                  passwordString <- liftEff (IxSignal.get passwordSignal)
                  password <- hashPassword
                    { password: passwordString
                    , salt: env.salt
                    }
                  OneIO.callAsync registerQueues $ RegisterInitIn {email,password,reCaptcha}
          _ -> pure unit
