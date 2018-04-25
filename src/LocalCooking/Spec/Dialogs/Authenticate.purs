module LocalCooking.Spec.Dialogs.Authenticate where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Types.Env (Env)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Links.Class (registerLink, toLocation, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Common.Password (HashedPassword, hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (genUUID, GENUUID)
import Data.Time.Duration (Milliseconds (..))
import Text.Email.Validate (EmailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal
import React.Icons (facebookIcon, twitterIcon, googleIcon)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)

import MaterialUI.Types (createStyles)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import Crypto.Scrypt (SCRYPT)

import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State siteLinks =
  { open        :: Boolean
  , windowSize  :: WindowSize
  , currentPage :: siteLinks
  }


initialState :: forall siteLinks
              . {initSiteLinks :: siteLinks, initWindowSize :: WindowSize} -> State siteLinks
initialState {initWindowSize, initSiteLinks} =
  { open: false
  , windowSize: initWindowSize
  , currentPage: initSiteLinks
  }


data Action siteLinks
  = Open
  | Close
  | ChangedWindowSize WindowSize
  | ChangedPage siteLinks
  | SubmitAuthenticate

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  , dom       :: DOM
  , history   :: HISTORY
  | eff)


spec :: forall eff siteLinks userDetailsLinks
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => { toURI :: Location -> URI
        , env :: Env
        , authenticateDialogOutputQueue :: One.Queue (write :: WRITE) (Effects eff) HashedPassword
        , password ::
          { signal       :: IxSignal (Effects eff) String
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          , errorQueue   :: One.Queue (write :: WRITE) (Effects eff) Unit
          }
        , submit ::
          { disabledSignal :: IxSignal (Effects eff) Boolean
          , queue          :: IxQueue (read :: READ) (Effects eff) Unit
          }
        , pendingSignal :: IxSignal (Effects eff) Boolean
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { toURI
  , env
  , password
  , submit
  , pendingSignal
  , authenticateDialogOutputQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        liftEff $ IxSignal.set false pendingSignal
        void $ T.cotransform _ { open = false }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff $ do
          IxSignal.set "" password.signal
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      SubmitAuthenticate -> do
        liftEff $ IxSignal.set true pendingSignal
        password <- liftEff (IxSignal.get password.signal)
        liftBase $ do
          hashedPassword <- hashPassword {salt: env.salt, password}
          liftEff $ One.putQueue authenticateDialogOutputQueue hashedPassword

    render :: T.Render (State siteLinks) Unit (Action siteLinks)
    render dispatch props state children =
      [ let dialog' =
              if state.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> do
                      pending <- unsafeCoerceEff $ IxSignal.get pendingSignal
                      when (not pending) (dispatch Close)
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Authenticate"]
            , dialogContent {style: createStyles {position: "relative"}}
              [ Password.password
                { label: R.text "Password"
                , fullWidth: true
                , name: "authenticate-password"
                , id: "authenticate-password"
                , passwordSignal: password.signal
                , parentSignal: Nothing
                , updatedQueue: password.updatedQueue
                , errorQueue: password.errorQueue
                }
              , pending
                { pendingSignal
                }
              ]
            , dialogActions {}
              [ Submit.submit
                { color: Button.primary
                , variant: Button.flat
                , size: Button.medium
                , style: createStyles {}
                , triggerQueue: submit.queue
                , disabledSignal: submit.disabledSignal
                } [R.text "Submit"]
              , button
                { color: Button.default
                , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Cancel"]
              ]
            ]
      ]



authenticateDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit HashedPassword
               , returnAuthenticateQueue :: One.Queue (write :: WRITE) (Effects eff) (Maybe Unit)
               , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
               , currentPageSignal :: IxSignal (Effects eff) siteLinks
               , toURI             :: Location -> URI
               , env               :: Env
               }
            -> R.ReactElement
authenticateDialog
  { authenticateDialogQueue: OneIO.IOQueues {input: authenticateDiaauthenticateputQueue, output: authenticateDialogOutputQueue}
  , returnAuthenticateQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { toURI
            , env
            , password:
              { signal: passwordSignal
              , updatedQueue: passwordQueue
              , errorQueue: passwordErrorQueue
              }
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              }
            , pendingSignal
            , authenticateDialogOutputQueue
            } )
          (initialState init)
      reactSpecAuthenticate =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Queue.whileMountedOne
            authenticateDiaauthenticateputQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
        $ Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitAuthenticate)
        $ Queue.whileMountedOne
            (One.allowReading returnAuthenticateQueue)
            (\this mErr -> case mErr of
                Nothing -> unsafeCoerceEff $ dispatcher this Close
                Just _ -> One.putQueue passwordErrorQueue unit
                )
            reactSpec
  in  R.createElement (R.createClass reactSpecAuthenticate) unit []
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Left ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    submitDisabledSignal = unsafePerformEff $ IxSignal.make false
    emailQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ One.writeOnly <$> One.newQueue
    pendingSignal = unsafePerformEff (IxSignal.make false)
    submitQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue

    _ = unsafePerformEff $ do
      k <- show <$> genUUID
      let submitValue = do
            mEmail <- IxSignal.get emailSignal
            x <- case mEmail of
              Right (Just _) -> do
                p1 <- IxSignal.get passwordSignal
                pure (p1 == "")
              _ -> pure true
            IxSignal.set x submitDisabledSignal
      IxQueue.onIxQueue emailQueue k \_ -> submitValue
      IxQueue.onIxQueue passwordQueue k \_ -> submitValue
      IxSignal.subscribe (\_ -> submitValue) emailSignal
      IxSignal.subscribe (\_ -> submitValue) passwordSignal
