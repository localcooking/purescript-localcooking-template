module LocalCooking.Spec.Content.UserDetails.Security where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Snackbar (SnackbarMessage (SnackbarMessageSecurity), SecurityMessage (..))
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams)
import LocalCooking.Common.User.Password (HashedPassword, hashPassword)
import LocalCooking.Dependencies.Common (SetUserSparrowClientQueues)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Semantics.Common (User (..))
-- import LocalCooking.Client.Dependencies.Security (SecuritySparrowClientQueues, SecurityInitIn' (..), SecurityInitOut' (..))
-- import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))
import Facebook.State (FacebookLoginUnsavedFormData (FacebookLoginUnsavedFormDataSecurity))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.UUID (GENUUID)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref.Extra (takeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (readOnly, writeOnly)
import Queue (WRITE, READ)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue




type State =
  { rerender :: Unit
  }

initialState :: State
initialState =
  { rerender: unit
  }

data Action
  = SubmitSecurity
  | ReRender


type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  , scrypt :: SCRYPT
  | eff)


spec :: forall eff siteLinks userDetails
      . LocalCookingParams siteLinks userDetails (Effects eff)
     -> { errorMessageQueue       :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , env                     :: Env
        , setUserQueues           :: SetUserSparrowClientQueues (Effects eff)
        , authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
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
        , pendingSignal            :: IxSignal (Effects eff) Boolean
        } -> T.Spec (Effects eff) State Unit Action
spec
  {authTokenSignal}
  { errorMessageQueue
  , env
  , setUserQueues
  , authenticateDialogQueue
  , email
  , emailConfirm
  , password
  , passwordConfirm
  , submit
  , pendingSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ReRender -> void $ T.cotransform _ {rerender = unit}
      SubmitSecurity -> do
        liftEff $ IxSignal.set true pendingSignal
        mAuthToken <- liftEff $ IxSignal.get authTokenSignal
        case mAuthToken of
          Just authToken -> do
            mEmail <- liftEff $ IxSignal.get email.signal
            case mEmail of
              Email.EmailGood email -> do
                mAuthPass <- liftBase $ OneIO.callAsync authenticateDialogQueue unit
                case mAuthPass of
                  Nothing -> pure unit
                  Just oldPassword -> do
                    mErr <- liftBase $ do
                      passwordString <- liftEff (IxSignal.get password.signal)
                      newPassword <- hashPassword
                        { password: passwordString
                        , salt: env.salt
                        }
                      pure Nothing -- FIXME
                      -- OneIO.callAsync setUserQueues $ AccessInitIn
                      --   { token: authToken
                      --   , subj: User {email,newPassword,oldPassword,fbUserId: Nothing}
                      --     -- FIXME facebook user id assignment
                      --   }
                    liftEff $ do
                      One.putQueue errorMessageQueue $ case mErr of
                        Nothing -> SnackbarMessageSecurity SecuritySaveFailed
                        Just JSONUnit -> SnackbarMessageSecurity SecuritySaveSuccess
                      IxSignal.set false pendingSignal
              _ -> pure unit
          _ -> pure unit

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
        , setQueue
        }
      , Email.email
        { label: R.text "Email Confirm"
        , fullWidth: true
        , name: "register-email-confirm"
        , id: "register-email-confirm"
        , emailSignal: emailConfirm.signal
        , parentSignal: Just email.signal
        , updatedQueue: emailConfirm.updatedQueue
        , setQueue: setConfirmQueue
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
        passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        passwordConfirmErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        setQueue = unsafePerformEff $ writeOnly <$> One.newQueue
        setConfirmQueue = unsafePerformEff $ writeOnly <$> One.newQueue


security :: forall eff siteLinks userDetails
          . LocalCookingParams siteLinks userDetails (Effects eff)
         -> { errorMessageQueue       :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
            , authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
            , setUserQueues           :: SetUserSparrowClientQueues (Effects eff)
            , env                     :: Env
            , initFormDataRef         :: Ref (Maybe FacebookLoginUnsavedFormData)
            }
         -> R.ReactElement
security
  params
  { errorMessageQueue
  , authenticateDialogQueue
  , env
  , setUserQueues
  , initFormDataRef
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { env
            , errorMessageQueue
            , authenticateDialogQueue
            , setUserQueues
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
          Queue.whileMountedIx
            submitQueue
            "onSubmit"
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitSecurity)
        $ Queue.whileMountedIx
            emailUpdatedQueue
            "emailUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            emailConfirmUpdatedQueue
            "emailConfirmUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            passwordUpdatedQueue
            "passwordUpdated"
            (\this _ -> submitValue this)
        $ Queue.whileMountedIx
            passwordConfirmUpdatedQueue
            "passwordConfirmUpdated"
            (\this _ -> submitValue this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    {emailSignal,emailConfirmSignal} = unsafePerformEff $ do
      mX <- takeRef initFormDataRef
      Tuple e1 e2 <- case mX of
        Just x -> do
          unsafeCoerceEff $ log "Taken by security..."
          case x of
            FacebookLoginUnsavedFormDataSecurity {email,emailConfirm} -> do
              unsafeCoerceEff $ log $ "sending... " <> email <> ", " <> emailConfirm
              pure (Tuple (Email.EmailPartial email) (Email.EmailPartial emailConfirm))
            _ -> pure (Tuple (Email.EmailPartial "") (Email.EmailPartial ""))
        _ -> pure (Tuple (Email.EmailPartial "") (Email.EmailPartial ""))
      a <- IxSignal.make e1
      b <- IxSignal.make e2
      pure {emailSignal: a, emailConfirmSignal: b}
    emailUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    emailConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordSignal = unsafePerformEff (IxSignal.make "")
    passwordUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordConfirmSignal = unsafePerformEff (IxSignal.make "")
    passwordConfirmUpdatedQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    submitDisabledSignal = unsafePerformEff (IxSignal.make true)
