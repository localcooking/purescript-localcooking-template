module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Snackbar (SnackbarMessage (..))
import LocalCooking.Types.Env (Env)
import LocalCooking.Auth.Error (AuthError (AuthExistsFailure))
import LocalCooking.Window (WindowSize)
import LocalCooking.Client.Dependencies.PasswordVerify (PasswordVerifySparrowClientQueues, PasswordVerifyInitIn (PasswordVerifyInitInUnauth), PasswordVerifyInitOut (PasswordVerifyInitOutSuccess))
import LocalCooking.Client.Dependencies.AuthToken (AuthTokenFailure (BadPassword))
import LocalCooking.Links (ThirdPartyLoginReturnLinks (..))
import LocalCooking.Links.Class (registerLink, toLocation, class LocalCookingSiteLinks, class ToLocation)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..))
import LocalCooking.Common.Password (HashedPassword, hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (genUUID, GENUUID)
import Text.Email.Validate (EmailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Icons (facebookIcon, twitterIcon, googleIcon)
import DOM (DOM)

import MaterialUI.Types (createStyles)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
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
  , console   :: CONSOLE
  , dom       :: DOM
  | eff)


loginDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { loginDialogQueue     :: OneIO.IOQueues (Effects eff) Unit (Maybe {email :: EmailAddress, password :: HashedPassword})
               , passwordVerifyQueues :: PasswordVerifySparrowClientQueues (Effects eff)
               , errorMessageQueue    :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
               , windowSizeSignal     :: IxSignal (Effects eff) WindowSize
               , currentPageSignal    :: IxSignal (Effects eff) siteLinks
               , toURI                :: Location -> URI
               , env                  :: Env
               , toRegister           :: Eff (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog
  { loginDialogQueue
  , passwordVerifyQueues
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  , toRegister
  } =
  genericDialog
  { dialogQueue: loginDialogQueue
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  , buttons: \{close} ->
    [ button
      { color: Button.secondary
      , onClick: mkEffFn1 preventDefault
      , onTouchTap: mkEffFn1 \e -> do
          preventDefault e
          unsafeCoerceEff $ do
            close
            toRegister
      , href: URI.print $ toURI $ toLocation $ registerLink :: siteLinks
      } [R.text "Register"]
    ]
  , title: "Login"
  , submitValue: "Submit"
  , pends: true
  , content:
    { component: \{submitDisabled} ->
      let _ = unsafePerformEff $ do
            k <- show <$> genUUID
            let submitValue = do
                  mEmail <- IxSignal.get emailSignal
                  x <- case mEmail of
                    Right (Just _) -> do
                      p1 <- IxSignal.get passwordSignal
                      pure (p1 == "")
                    _ -> pure true
                  submitDisabled x
            IxQueue.onIxQueue emailQueue k \_ -> submitValue
            IxQueue.onIxQueue passwordQueue k \_ -> submitValue
            IxSignal.subscribe (\_ -> submitValue) emailSignal
            IxSignal.subscribe (\_ -> submitValue) passwordSignal
      in  [ Email.email
            { label: R.text "Email"
            , fullWidth: true
            , name: "login-email"
            , id: "login-email"
            , emailSignal: emailSignal
            , parentSignal: Nothing
            , updatedQueue: emailQueue
            }
          , Password.password
            { label: R.text "Password"
            , fullWidth: true
            , name: "login-password"
            , id: "login-password"
            , passwordSignal: passwordSignal
            , parentSignal: Nothing
            , updatedQueue: passwordQueue
            , errorQueue: passwordErrorQueue
            }
          , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em"}] $
              let mkFab mainColor darkColor icon mLink =
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
                            _ -> false
                          , href: case mLink of
                            Nothing -> ""
                            Just link -> URI.print $ facebookLoginLinkToURI env link
                          } [icon]
                      )
              in  [ mkFab "#3b5998" "#1e3f82" facebookIcon $
                      Just $ FacebookLoginLink
                      { redirectURL: toURI (toLocation FacebookLoginReturn)
                      , state: FacebookLoginState
                        { origin: unsafePerformEff (IxSignal.get currentPageSignal)
                        }
                      }
                  , mkFab "#1da1f3" "#0f8cdb" twitterIcon Nothing
                  , mkFab "#dd4e40" "#c13627" googleIcon Nothing
                  ]
          ]
    , obtain: do
      mEmail <- liftEff (IxSignal.get emailSignal)
      case mEmail of
        Right (Just email) -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: env.salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyQueues
            (PasswordVerifyInitInUnauth {email,password: hashedPassword})
          case mVerify of
            Just PasswordVerifyInitOutSuccess -> do
              pure (Just {email,password: hashedPassword}) -- FIXME delay until other queues are finished - user details, auth token, etc.
            _ -> do
              liftEff $ case mVerify of
                Nothing ->
                  One.putQueue errorMessageQueue (SnackbarMessageAuthError AuthExistsFailure)
                _ ->
                  One.putQueue errorMessageQueue (SnackbarMessageAuthFailure BadPassword)
              liftEff $ One.putQueue passwordErrorQueue unit
              pure Nothing
        _ -> do
          liftEff $ log "bad email!" -- FIXME bug out somehow?
          pure Nothing
    , reset: do
      IxSignal.set (Left "") emailSignal
      IxSignal.set "" passwordSignal
    }
  }
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Left ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    emailQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
