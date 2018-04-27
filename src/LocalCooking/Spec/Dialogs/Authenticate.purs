module LocalCooking.Spec.Dialogs.Authenticate where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Snackbar (SnackbarMessage (..))
import LocalCooking.Types.Env (Env)
import LocalCooking.Window (WindowSize)
import LocalCooking.Links.Class (class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Common.Password (HashedPassword, hashPassword)
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Client.Dependencies.PasswordVerify (PasswordVerifySparrowClientQueues, PasswordVerifyInitIn (PasswordVerifyInitInAuth), PasswordVerifyInitOut (PasswordVerifyInitOutSuccess))
import LocalCooking.Client.Dependencies.AuthToken (AuthTokenFailure (BadPassword))
import LocalCooking.Auth.Error (AuthError (AuthExistsFailure))

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.UUID (genUUID, GENUUID)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import DOM (DOM)

import Crypto.Scrypt (SCRYPT)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , dom       :: DOM
  | eff)


authenticateDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
               , passwordVerifyQueues    :: PasswordVerifySparrowClientQueues (Effects eff)
               , errorMessageQueue       :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
               , windowSizeSignal        :: IxSignal (Effects eff) WindowSize
               , currentPageSignal       :: IxSignal (Effects eff) siteLinks
               , authTokenSignal         :: IxSignal (Effects eff) (Maybe AuthToken)
               , toURI                   :: Location -> URI
               , env                     :: Env
               }
            -> R.ReactElement
authenticateDialog
  { authenticateDialogQueue
  , passwordVerifyQueues
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , authTokenSignal
  , toURI
  , env
  } =
  genericDialog
  { dialogQueue: authenticateDialogQueue
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  , buttons: \_ -> []
  , title: "Authenticate"
  , submitValue: "Submit"
  , pends: true
  , content:
    { component: \{submitDisabled} ->
      let _ = unsafePerformEff $ do
            k <- show <$> genUUID
            let submitValue = do
                  p1 <- IxSignal.get passwordSignal
                  submitDisabled (p1 == "")
            IxQueue.onIxQueue passwordQueue k \_ -> submitValue
            IxSignal.subscribe (\_ -> submitValue) passwordSignal
      in  [ Password.password
            { label: R.text "Password"
            , fullWidth: true
            , name: "authenticate-password"
            , id: "authenticate-password"
            , passwordSignal
            , parentSignal: Nothing
            , updatedQueue: passwordQueue
            , errorQueue: passwordErrorQueue
            }
          ]
    , obtain: do
      mAuthToken <- liftEff (IxSignal.get authTokenSignal)
      case mAuthToken of
        Nothing -> pure Nothing
        Just authToken -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: env.salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyQueues
            (PasswordVerifyInitInAuth {authToken,password: hashedPassword})
          case mVerify of
            Just PasswordVerifyInitOutSuccess -> do
              pure (Just hashedPassword)
            _ -> do
              liftEff $ case mVerify of
                Nothing ->
                  One.putQueue errorMessageQueue (SnackbarMessageAuthError AuthExistsFailure)
                _ ->
                  One.putQueue errorMessageQueue (SnackbarMessageAuthFailure BadPassword)
              liftEff (One.putQueue passwordErrorQueue unit)
              pure Nothing
    , reset: do
      IxSignal.set "" passwordSignal
    }
  }
  where
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    passwordQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
