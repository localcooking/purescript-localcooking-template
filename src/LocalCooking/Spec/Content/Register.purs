module LocalCooking.Spec.Content.Register where

import LocalCooking.Spec.Form.Email (email)
import LocalCooking.Spec.Snackbar (SnackbarMessage (SnackbarMessageRegister), RegisterError (..))
import LocalCooking.Types.Env (Env)
import Google.ReCaptcha (ReCaptchaResponse)
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues, RegisterInitIn (..), RegisterInitOut (..))
import LocalCooking.Common.Password (hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.ReCaptcha (reCaptcha)
import React.Signal.WhileMounted as Signal

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
import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO


type State =
  { reCaptcha            :: Maybe ReCaptchaResponse
  , email                :: Maybe (Maybe EmailAddress)
  , emailConfirm         :: Maybe (Maybe EmailAddress)
  -- , email                :: String
  -- , emailDirty           :: Maybe Boolean
  -- , emailConfirm         :: String
  -- , emailConfirmDirty    :: Maybe Boolean
  , password             :: String
  , passwordDirty        :: Maybe Boolean
  , passwordConfirm      :: String
  , passwordConfirmDirty :: Maybe Boolean
  , pending              :: Boolean
  }

initialState :: State
initialState =
  { reCaptcha: Nothing
  , email: Nothing
  , emailConfirm: Nothing
  -- , email: ""
  -- , emailDirty: Nothing
  -- , emailConfirm: ""
  -- , emailConfirmDirty: Nothing
  , password: ""
  , passwordDirty: Nothing
  , passwordConfirm: ""
  , passwordConfirmDirty: Nothing
  , pending: false
  }

data Action
  = GotReCaptchaVerify ReCaptchaResponse
  | ChangedEmail (Maybe (Maybe EmailAddress))
  -- | ChangedEmail String
  | ChangedEmailConfirm (Maybe (Maybe EmailAddress))
  -- | ChangedEmailConfirm String
  -- | EmailUnfocused
  -- | EmailConfirmUnfocused
  | ChangedPassword String
  | ChangedPasswordConfirm String
  | PasswordUnfocused
  | PasswordConfirmUnfocused
  | SubmitRegister


type Effects eff =
  ( ref       :: REF
  , scrypt    :: SCRYPT
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  | eff)


spec :: forall eff
      . { registerQueues :: RegisterSparrowClientQueues (Effects eff)
        , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , toRoot :: Eff (Effects eff) Unit
        , env :: Env
        , emailSignal :: IxSignal (Effects eff) (Maybe (Maybe EmailAddress))
        , emailConfirmSignal :: IxSignal (Effects eff) (Maybe (Maybe EmailAddress))
        } -> T.Spec (Effects eff) State Unit Action
spec
  { registerQueues
  , errorMessageQueue
  , toRoot
  , env
  , emailSignal
  , emailConfirmSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotReCaptchaVerify e -> void $ T.cotransform _ { reCaptcha = Just e }
      ChangedEmail e -> void $ T.cotransform _ { email = e }
      ChangedEmailConfirm e -> void $ T.cotransform _ { emailConfirm = e }
      -- EmailUnfocused -> void $ T.cotransform _ { emailDirty = Just true }
      -- EmailConfirmUnfocused -> void $ T.cotransform _ { emailConfirmDirty = Just true }
      ChangedPassword e -> void $ T.cotransform _ { password = e, passwordDirty = Just false }
      ChangedPasswordConfirm e -> void $ T.cotransform _ { passwordConfirm = e, passwordConfirmDirty = Just false }
      PasswordUnfocused -> void $ T.cotransform _ { passwordDirty = Just true }
      PasswordConfirmUnfocused -> void $ T.cotransform _ { passwordConfirmDirty = Just true }
      SubmitRegister -> do
        void $ T.cotransform _ { pending = true }
        case state.email of
          Just (Just email) -> case state.reCaptcha of
            Nothing -> pure unit
            Just reCaptcha -> do
              mErr <- liftBase $ do
                password <- hashPassword {password: state.password, salt: env.salt}
                OneIO.callAsync registerQueues $ RegisterInitIn {email,password,reCaptcha}
              void $ T.cotransform _ { pending = false }
              case mErr of
                Nothing -> pure unit
                Just initOut -> case initOut of
                  RegisterInitOutEmailSent ->
                    liftEff $ One.putQueue errorMessageQueue (SnackbarMessageRegister Nothing)
                  RegisterInitOutBadCaptcha ->
                    liftEff $ One.putQueue errorMessageQueue $ SnackbarMessageRegister $ Just RegisterErrorBadCaptchaResponse
                  RegisterInitOutDBError e ->
                    liftEff $ One.putQueue errorMessageQueue $ SnackbarMessageRegister $ Just RegisterErrorEmailInUse
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
          -- [ textField
          --   { label: R.text "Email"
          --   , fullWidth: true
          --   , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
          --   , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
          --   , error: case emailAddress state.email of
          --       Nothing -> state.emailDirty == Just true
          --       Just _ -> case state.emailConfirmDirty of
          --         Nothing -> false
          --         Just dirty
          --           | dirty -> state.email /= state.emailConfirm
          --           | otherwise -> false
          --   , name: "register-email"
          --   , id: "register-email"
          --   }
          [ email
            { label: R.text "Email"
            , fullWidth: true
            , name: "register-email"
            , id: "register-email"
            , emailSignal
            , parentSignal: Nothing
            }
          , email
            { label: R.text "Email Confirm"
            , fullWidth: true
            , name: "register-email-confirm"
            , id: "register-email-confirm"
            , emailSignal: emailConfirmSignal
            , parentSignal: Just emailSignal
            }
          -- , textField
          --   { label: R.text "Email Confirm"
          --   , fullWidth: true
          --   , onChange: mkEffFn1 \e -> dispatch $ ChangedEmailConfirm (unsafeCoerce e).target.value
          --   , onBlur: mkEffFn1 \_ -> dispatch EmailConfirmUnfocused
          --   , error: case emailAddress state.emailConfirm of
          --       Nothing -> state.emailConfirmDirty == Just true
          --       Just _ -> case state.emailDirty of
          --         Nothing -> false
          --         Just _ -> state.email /= state.emailConfirm
          --   , name: "register-email-confirm"
          --   , id: "register-email-confirm"
          --   }
          , textField
            { label: R.text "Password"
            , fullWidth: true
            , "type": Input.passwordType
            , onChange: mkEffFn1 \p -> dispatch $ ChangedPassword (unsafeCoerce p).target.value
            , onBlur: mkEffFn1 \_ -> dispatch PasswordUnfocused
            , error: case state.passwordDirty of
                Nothing -> false
                Just dirty
                  | not dirty -> false
                  | otherwise -> case state.passwordConfirmDirty of
                    Nothing -> false
                    Just dirty
                      | not dirty -> false
                      | otherwise -> state.password /= state.passwordConfirm
            , name: "register-password"
            , id: "register-password"
            }
          , textField
            { label: R.text "Password Confirm"
            , fullWidth: true
            , "type": Input.passwordType
            , onChange: mkEffFn1 \p -> dispatch $ ChangedPasswordConfirm (unsafeCoerce p).target.value
            , onBlur: mkEffFn1 \_ -> dispatch PasswordConfirmUnfocused
            , error: case state.passwordConfirmDirty of
                Nothing -> false
                Just dirty
                  | not dirty -> false
                  | otherwise -> case state.passwordDirty of
                    Nothing -> false
                    Just dirty
                      | not dirty -> false
                      | otherwise -> state.password /= state.passwordConfirm
            , name: "register-password-confirm"
            , id: "register-password-confirm"
            , style: createStyles {marginBottom: "1em"}
            }
          , reCaptcha
            { sitekey: env.googleReCaptchaSiteKey
            , verifyCallback: mkEffFn1 (dispatch <<< GotReCaptchaVerify)
            , onloadCallback: pure unit
            }
          , button
            { color: Button.secondary
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {marginTop: "1em"}
            , disabled: case state.email of
              Just (Just _) ->
                   state.email /= state.emailConfirm
                || state.password /= state.passwordConfirm
                || case state.passwordDirty of
                      Nothing -> true
                      Just _ -> case state.reCaptcha of
                        Nothing -> true
                        Just _ -> false
              _ -> true
            , onTouchTap: mkEffFn1 \_ -> dispatch SubmitRegister
            } [R.text "Submit"]
          ]
        ]
      , if state.pending
          then R.div
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
          else R.text ""
      ]


register :: forall eff
          . { registerQueues    :: RegisterSparrowClientQueues (Effects eff)
            , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
            , toRoot            :: Eff (Effects eff) Unit
            , env               :: Env
            }
         -> R.ReactElement
register {registerQueues,errorMessageQueue,toRoot,env} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { registerQueues
            , errorMessageQueue
            , toRoot
            , env
            , emailSignal
            , emailConfirmSignal
            }
            ) initialState
      reactSpec' =
          Signal.whileMountedIxUUID
            emailSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedEmail x))
        $ Signal.whileMountedIxUUID
            emailConfirmSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedEmailConfirm x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    emailSignal = unsafePerformEff (IxSignal.make Nothing)
    emailConfirmSignal = unsafePerformEff (IxSignal.make Nothing)
