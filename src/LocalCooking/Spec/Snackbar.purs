module LocalCooking.Spec.Snackbar where

import LocalCooking.Client.Dependencies.AuthToken (AuthTokenFailure (..))
import LocalCooking.Auth.Error (AuthError (..))

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.Generic (class Generic, gShow)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Uncurried (mkEffFn2, mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)

import MaterialUI.Snackbar (snackbar)
import MaterialUI.IconButton (iconButton)
import MaterialUI.Icons.Close (closeIcon)

import Queue (READ)
import Queue.One as One



data UserEmailError
  = UserEmailNoInitOut
  | UserEmailNoAuth

derive instance genericUserEmailError :: Generic UserEmailError

instance showUserEmailError :: Show UserEmailError where
  show = gShow


data RegisterError
  = RegisterErrorBadCaptchaResponse
  | RegisterErrorEmailInUse

derive instance genericRegisterError :: Generic RegisterError

instance showRegisterError :: Show RegisterError where
  show = gShow


data RedirectError
  = RedirectRegisterAuth
  | RedirectUserDetailsNoAuth

derive instance genericRedirectError :: Generic RedirectError

instance showRedirectError :: Show RedirectError where
  show = gShow


data SnackbarMessage
  = SnackbarMessageAuthFailure AuthTokenFailure
  | SnackbarMessageAuthError AuthError
  | SnackbarMessageUserEmail UserEmailError
  | SnackbarMessageRegister (Maybe RegisterError)
  | SnackbarMessageRedirect RedirectError

derive instance genericSnackbarMessage :: Generic SnackbarMessage

instance showSnackbarMessage :: Show SnackbarMessage where
  show = gShow



type State =
  { errors :: Array SnackbarMessage
  , open :: Boolean
  }

initialState :: State
initialState =
  { errors: []
  , open: false
  }

data Action
  = GotMessage SnackbarMessage
  | PopMessage

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotMessage x -> do
        liftEff $ unsafeCoerceEff $ log $ "got message: " <> show x
        void $ T.cotransform _ { errors = Array.snoc state.errors x, open = true }
      PopMessage -> do
        case Array.uncons state.errors of
          Nothing -> pure unit
          Just {head,tail} -> do
            void $ T.cotransform _ { open = false }
            liftBase $ delay $ Milliseconds 2000.0
            void $ T.cotransform _ { errors = tail }
            unless (Array.null tail) $ do
              liftBase $ delay $ Milliseconds 1000.0
              void $ T.cotransform _ { open = true }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ snackbar
        { open: state.open
        , autoHideDuration: toNullable $ Just $ Milliseconds 10000.0
        -- , resumeHideDuration: toNullable $ Just $ Milliseconds 0.0
        , onClose: mkEffFn2 \_ _ -> dispatch PopMessage
        , message: R.div []
          [ case Array.head state.errors of
              Nothing -> R.text ""
              Just x -> case x of
                SnackbarMessageAuthFailure authFailure -> case authFailure of
                  BadPassword -> R.text "Password incorrect, please try again."
                  EmailDoesntExist -> R.text "Email address not found, please register."
                SnackbarMessageAuthError authError -> case authError of
                  FBLoginReturnBad code msg -> R.text $ "Bad Facebook login response: " <> code <> ", " <> msg
                  FBLoginReturnDenied desc -> R.text $ "Facebook login denied: " <> desc
                  FBLoginReturnBadParse -> R.text "Internal error: Facebook login return unparsable."
                  FBLoginReturnNoUser -> R.text "Facebook user not recognized, please link your account."
                  AuthExistsFailure -> R.text "Warning: You've been logged out; your session expired."
                SnackbarMessageUserEmail userEmail -> case userEmail of
                  UserEmailNoInitOut -> R.text "Internal Error: userEmail resource failed"
                  UserEmailNoAuth -> R.text "Error: No authorization for email"
                SnackbarMessageRegister mRegister -> case mRegister of
                  Nothing -> R.text "Registered! Please check your spam folder and confirm in 7 days."
                  Just register -> case register of
                    RegisterErrorBadCaptchaResponse -> R.text "Bad ReCaptcha response."
                    RegisterErrorEmailInUse -> R.text "Email address is already registered."
                SnackbarMessageRedirect redirect -> case redirect of
                  RedirectRegisterAuth -> R.text "Redirected - can't register while logged in."
                  RedirectUserDetailsNoAuth -> R.text "Redirected - can't view user details while logged out."
          ]
        , action:
          [ iconButton
            { onTouchTap: mkEffFn1 \_ -> dispatch PopMessage
            } closeIcon
          ]
        }
      ]



messages :: forall eff
          . { errorMessageQueue :: One.Queue (read :: READ) (Effects eff) SnackbarMessage
            } -> R.ReactElement
messages {errorMessageQueue} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' =
        Queue.whileMountedOne
          errorMessageQueue
          (\this x -> unsafeCoerceEff $ dispatcher this (GotMessage x))
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
