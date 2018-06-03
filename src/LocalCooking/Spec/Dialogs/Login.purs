module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Snackbar (SnackbarMessage (..))
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Dependencies.Validate (PasswordVerifyUnauthSparrowClientQueues, PasswordVerifyUnauth (..))
import LocalCooking.Dependencies.AuthToken (AuthTokenFailure (AuthTokenLoginFailure))
import LocalCooking.Links (ThirdPartyLoginReturnLinks (..))
import LocalCooking.Links.Class (registerLink, toLocation, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Common.User.Password (HashedPassword, hashPassword)
import Facebook.Types (FacebookClientId)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI.URI (print) as URI
import Data.UUID (genUUID, GENUUID)
import Data.Argonaut.JSONUnit (JSONUnit (..))
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

import Queue.Types (readOnly, writeOnly)
import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue as IxQueue
import IxSignal.Internal as IxSignal




type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  , dom       :: DOM
  | eff)


loginDialog :: forall eff siteLinks userDetails userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => LocalCookingParams siteLinks userDetails (Effects eff)
            -> { loginDialogQueue     :: OneIO.IOQueues (Effects eff) Unit (Maybe {email :: EmailAddress, password :: HashedPassword})
               , loginCloseQueue      :: One.Queue (write :: WRITE) (Effects eff) Unit
               , passwordVerifyUnauthQueues :: PasswordVerifyUnauthSparrowClientQueues (Effects eff)
               , errorMessageQueue    :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
               , env                  :: Env
               , toRegister           :: Eff (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog
  params@{toURI,currentPageSignal}
  { loginDialogQueue
  , loginCloseQueue
  , passwordVerifyUnauthQueues
  , errorMessageQueue
  , env: {facebookClientId, salt}
  , toRegister
  } =
  genericDialog
  params
  { dialogQueue: loginDialogQueue
  , closeQueue: Just loginCloseQueue
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
                    Email.EmailGood _ -> do
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
            , setQueue
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
              [ mkFab facebookClientId "#3b5998" "#1e3f82" facebookIcon $ Just $ FacebookLoginLink
                { redirectURL: toURI (toLocation FacebookLoginReturn)
                , state: FacebookLoginState
                  { origin: unsafePerformEff (IxSignal.get currentPageSignal)
                  , formData: Nothing
                  }
                }
              , mkFab facebookClientId "#1da1f3" "#0f8cdb" twitterIcon Nothing
              , mkFab facebookClientId "#dd4e40" "#c13627" googleIcon Nothing
              ]
          ]
    , obtain: do
      mEmail <- liftEff (IxSignal.get emailSignal)
      case mEmail of
        Email.EmailGood email -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          hashedPassword <- liftBase (hashPassword {salt: salt, password: pw})
          mVerify <- OneIO.callAsync
            passwordVerifyUnauthQueues
            (PasswordVerifyUnauth {email,password: hashedPassword})
          case mVerify of -- FIXME nonexistent auth token error message?
            Just JSONUnit -> do
              pure (Just {email,password: hashedPassword}) -- FIXME delay until other queues are finished - user details, auth token, etc.
            _ -> do
              -- liftEff $ case mVerify of
              --   Nothing ->
              --     One.putQueue errorMessageQueue (SnackbarMessageAuthFailure AuthExistsFailure)
              --   _ ->
              liftEff $ do
                One.putQueue errorMessageQueue (SnackbarMessageAuthFailure AuthTokenLoginFailure)
                One.putQueue passwordErrorQueue unit
              pure Nothing
        _ -> do
          liftEff $ log "bad email!" -- FIXME bug out somehow?
          pure Nothing
    , reset: do
      IxSignal.set (Email.EmailPartial "") emailSignal
      IxSignal.set "" passwordSignal
    }
  }
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    emailQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    setQueue = unsafePerformEff $ writeOnly <$> One.newQueue


-- | For social logins
mkFab :: forall siteLinks
       . ToLocation siteLinks
      => FacebookClientId -> String -> String -> R.ReactElement
      -> Maybe (FacebookLoginLink siteLinks) -> R.ReactElement
mkFab facebookClientId mainColor darkColor icon mLink =
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
          Just link -> URI.print $
            facebookLoginLinkToURI facebookClientId link
        } [icon]
    )
