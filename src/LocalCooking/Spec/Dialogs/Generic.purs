module LocalCooking.Spec.Dialogs.Generic where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Snackbar (SnackbarMessage (..))
import LocalCooking.Types.Env (Env)
import LocalCooking.Auth.Error (AuthError (AuthExistsFailure))
import LocalCooking.Window (WindowSize (..))
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
import Data.Time.Duration (Milliseconds (..))
import Text.Email.Validate (EmailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, delay)
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
  | Submit

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  , dom       :: DOM
  , history   :: HISTORY
  | eff)


spec :: forall eff siteLinks userDetailsLinks output
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => { toURI :: Location -> URI
        , env :: Env
        , dialogOutputQueue :: One.Queue (write :: WRITE) (Effects eff) (Maybe output)
        , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
        , content ::
          { component ::
            { submitDisabled :: Boolean -> Eff (Effects eff) Unit
            } -> Array R.ReactElement
          , obtain    :: Aff (Effects eff) (Maybe output)
          , reset     :: Eff (Effects eff) Unit
          }
        , buttons ::
          { close :: Eff (Effects eff) Unit
          } -> Array R.ReactElement
        , title :: String
        , submit ::
          { disabledSignal :: IxSignal (Effects eff) Boolean
          , queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , value          :: String
          }
        , pendingSignal :: Maybe (IxSignal (Effects eff) Boolean)
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { toURI
  , env
  , submit
  , content
  , pendingSignal
  , dialogOutputQueue
  , buttons
  , title
  , errorMessageQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p  -> liftEff (IxSignal.set false p)
        void $ T.cotransform _ { open = false }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff content.reset
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      Submit -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p -> liftEff (IxSignal.set true p)
        mOutput <- liftBase content.obtain
        case mOutput of
          Nothing -> pure unit -- FIXME error out?
          Just output -> do
            performAction Close props state
            liftEff (One.putQueue dialogOutputQueue (Just output))

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
                      pending <- do
                        case pendingSignal of
                          Nothing -> pure false
                          Just p  -> unsafeCoerceEff (IxSignal.get p)
                      when (not pending) $ do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text title]
            , dialogContent {style: createStyles {position: "relative"}} $
                content.component
                { submitDisabled: \d -> IxSignal.set d submit.disabledSignal
                } <>
                  case pendingSignal of
                    Nothing -> []
                    Just p  ->
                      [ pending
                        { pendingSignal: p
                        }
                      ]
            , dialogActions {} $
                buttons
                { close: do
                    unsafeCoerceEff (dispatch Close)
                    One.putQueue dialogOutputQueue Nothing
                } <>
                  [ Submit.submit
                    { color: Button.primary
                    , variant: Button.flat
                    , size: Button.medium
                    , style: createStyles {}
                    , triggerQueue: submit.queue
                    , disabledSignal: submit.disabledSignal
                    } [R.text submit.value]
                  , button
                    { color: Button.default
                    , onTouchTap: mkEffFn1 \_ -> do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                    } [R.text "Cancel"]
                  ]
            ]
      ]



genericDialog :: forall eff siteLinks userDetailsLinks output
               . LocalCookingSiteLinks siteLinks userDetailsLinks
              => ToLocation siteLinks
              => { dialogQueue       :: OneIO.IOQueues (Effects eff) Unit (Maybe output)
                 , errorMessageQueue :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
                 , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
                 , currentPageSignal :: IxSignal (Effects eff) siteLinks
                 , toURI             :: Location -> URI
                 , env               :: Env
                 , buttons           ::
                    { close :: Eff (Effects eff) Unit
                    } -> Array R.ReactElement
                 , title             :: String
                 , submitValue       :: String
                 , pends             :: Boolean
                 , content ::
                   { component ::
                     { submitDisabled :: Boolean -> Eff (Effects eff) Unit
                     } -> Array R.ReactElement
                   , obtain    :: Aff (Effects eff) (Maybe output)
                   , reset     :: Eff (Effects eff) Unit
                   }
                 }
              -> R.ReactElement
genericDialog
  { dialogQueue: OneIO.IOQueues {input: dialogInputQueue, output: dialogOutputQueue}
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  , content
  , submitValue
  , buttons
  , title
  , pends
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
            , errorMessageQueue
            , buttons
            , title
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              , value: submitValue
              }
            , content
            , pendingSignal
            , dialogOutputQueue
            } )
          (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Queue.whileMountedOne
            dialogInputQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
        $ Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Submit)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    submitDisabledSignal = unsafePerformEff $ IxSignal.make false
    pendingSignal = if pends then unsafePerformEff (Just <$> IxSignal.make false) else Nothing
    submitQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
