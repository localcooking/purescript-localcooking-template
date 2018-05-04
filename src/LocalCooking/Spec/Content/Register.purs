module LocalCooking.Spec.Content.Register where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Google.ReCaptcha (reCaptcha)
import LocalCooking.Spec.Snackbar (SnackbarMessage (SnackbarMessageRegister), RegisterError (..))
import LocalCooking.Types.Env (Env)
import LocalCooking.Links (ThirdPartyLoginReturnLinks (..))
import LocalCooking.Links.Class (class ToLocation, toLocation)
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues, RegisterInitIn (..), RegisterInitOut (..))
import LocalCooking.Common.Password (hashPassword)
import Google.ReCaptcha (ReCaptchaResponse)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..), FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataRegister))
import Facebook.Types (FacebookUserId)

import Prelude
import Data.Maybe (Maybe (..), isJust)
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.UUID (genUUID, GENUUID)
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref.Extra (takeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER, setTimeout)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Icons (facebookIcon, twitterIcon, googleIcon)

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue


type State =
  { rerender :: Unit
  , fbUserId :: Maybe FacebookUserId
  }

initialState :: {initFbUserId :: Maybe FacebookUserId} -> State
initialState {initFbUserId} =
  { rerender: unit
  , fbUserId: initFbUserId
  }

data Action
  = SubmitRegister
  | ReRender


type Effects eff =
  ( ref       :: REF
  , scrypt    :: SCRYPT
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , timer     :: TIMER
  | eff)


spec :: forall eff siteLinks
      . ToLocation siteLinks
     => { registerQueues    :: RegisterSparrowClientQueues (Effects eff)
        , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , toRoot            :: Eff (Effects eff) Unit
        , env               :: Env
        , toURI             :: Location -> URI
        , currentPageSignal :: IxSignal (Effects eff) siteLinks
        , email ::
          { signal        :: IxSignal (Effects eff) Email.EmailState
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , emailConfirm ::
          { signal        :: IxSignal (Effects eff) Email.EmailState
          , updatedQueue  :: IxQueue (read :: READ) (Effects eff) Unit
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
  , toURI
  , currentPageSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ReRender -> void $ T.cotransform _ {rerender = unit}
      SubmitRegister -> do
        liftEff $ IxSignal.set true pendingSignal
        mEmail <- liftEff $ IxSignal.get email.signal
        case mEmail of
          Email.EmailGood email -> do
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
          , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em", paddingBottom: "2em"}] $
              let mkFab mainColor darkColor icon disabled mLink =
                    Button.withStyles
                      (\theme ->
                        { root: createStyles
                          { backgroundColor: mainColor
                          , color: "#ffffff"
                          , "&:hover": {backgroundColor: darkColor}
                          }
                        }
                      )
                      (\{classes} ->
                        button
                          { variant: Button.fab
                          , classes: Button.createClasses {root: classes.root}
                          , disabled: case mLink of
                            Nothing -> true
                            _ -> disabled
                          , href: case mLink of
                            Nothing -> ""
                            Just link -> URI.print (facebookLoginLinkToURI env link)
                          } [icon]
                      )
              in  [ mkFab "#3b5998" "#1e3f82" facebookIcon (isJust state.fbUserId) $
                      Just $ FacebookLoginLink
                      { redirectURL: toURI (toLocation FacebookLoginReturn)
                      , state: FacebookLoginState
                        { origin: unsafePerformEff (IxSignal.get currentPageSignal)
                        , formData: Just $ FacebookLoginUnsavedFormDataRegister
                          { email: case unsafePerformEff (IxSignal.get email.signal) of
                              Email.EmailPartial e -> e
                              Email.EmailBad e -> e
                              Email.EmailGood e -> Email.toString e
                          , emailConfirm: case unsafePerformEff (IxSignal.get emailConfirm.signal) of
                              Email.EmailPartial e -> e
                              Email.EmailBad e -> e
                              Email.EmailGood e -> Email.toString e
                          , fbUserId: Nothing
                          }
                        }
                      }
                  , mkFab "#1da1f3" "#0f8cdb" twitterIcon true Nothing
                  , mkFab "#dd4e40" "#c13627" googleIcon true Nothing
                  ]
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


register :: forall eff siteLinks
          . ToLocation siteLinks
         => { registerQueues    :: RegisterSparrowClientQueues (Effects eff)
            , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
            , toRoot            :: Eff (Effects eff) Unit
            , env               :: Env
            , initFormDataRef   :: Ref (Maybe FacebookLoginUnsavedFormData)
            , toURI             :: Location -> URI
            , currentPageSignal :: IxSignal (Effects eff) siteLinks
            }
         -> R.ReactElement
register
  { registerQueues
  , errorMessageQueue
  , toRoot
  , env
  , initFormDataRef
  , toURI
  , currentPageSignal
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { registerQueues
            , errorMessageQueue
            , toRoot
            , env
            , toURI
            , currentPageSignal
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
            , reCaptchaSignal
            , pendingSignal
            } ) (initialState {initFbUserId: fbUserId})
      submitValue this = do
        mEmail <- IxSignal.get emailSignal
        confirm <- IxSignal.get emailConfirmSignal
        x <- case mEmail of
          Email.EmailGood _ -> do
            p1 <- IxSignal.get passwordSignal
            if p1 == ""
              then pure true
              else do
                p2 <- IxSignal.get passwordConfirmSignal
                pure (mEmail /= confirm || p1 /= p2)
          _ -> pure true
        IxSignal.set x submitDisabledSignal
        unsafeCoerceEff $ dispatcher this ReRender
      reactSpec' =
          Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitRegister)
        $ Queue.whileMountedIxUUID
            emailUpdatedQueue
            (\this _ -> submitValue this)
        $ Queue.whileMountedIxUUID
            emailConfirmUpdatedQueue
            (\this _ -> submitValue this)
        $ Queue.whileMountedIxUUID
            passwordUpdatedQueue
            (\this _ -> submitValue this)
        $ Queue.whileMountedIxUUID
            passwordConfirmUpdatedQueue
            (\this _ -> submitValue this)
        -- $ Signal.whileMountedIxUUID
        --     emailSignal
        --     (\this _ -> submitValue this)
        -- $ Signal.whileMountedIxUUID
        --     emailConfirmSignal
        --     (\this _ -> submitValue this)
        -- $ Signal.whileMountedIxUUID
        --     passwordSignal
        --     (\this _ -> submitValue this)
        -- $ Signal.whileMountedIxUUID
        --     passwordConfirmSignal
        --     (\this _ -> submitValue this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    {emailSignal,emailConfirmSignal,fbUserId} = unsafePerformEff $ do
      mX <- takeRef initFormDataRef
      let {email,emailConfirm,fbUserId:fbUserId'} = case mX of
            Just x -> case x of
              FacebookLoginUnsavedFormDataRegister {email,emailConfirm,fbUserId} -> do
                { email: Email.EmailPartial email
                , emailConfirm: Email.EmailPartial emailConfirm
                , fbUserId
                }
              _ ->
                { email: Email.EmailPartial ""
                , emailConfirm: Email.EmailPartial ""
                , fbUserId: Nothing
                }
            _ ->
              { email: Email.EmailPartial ""
              , emailConfirm: Email.EmailPartial ""
              , fbUserId: Nothing
              }
      a <- IxSignal.make email
      b <- IxSignal.make emailConfirm
      unsafeCoerceEff $ log $ "From register: " <> show fbUserId'
      pure {emailSignal: a, emailConfirmSignal: b,fbUserId:fbUserId'}
    emailUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmUpdatedQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    reCaptchaSignal = unsafePerformEff (IxSignal.make Nothing)
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
    _ = unsafePerformEff $ log "rendering register.."
