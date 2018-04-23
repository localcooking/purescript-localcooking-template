module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Types.Env (Env)
import LocalCooking.Window (WindowSize (..))
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
  | SubmitLogin
  | ClickedRegister

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
        , toRegister :: Eff (Effects eff) Unit
        , loginDialogOutputQueue :: One.Queue (write :: WRITE) (Effects eff) {email :: EmailAddress, password :: HashedPassword}
        , email ::
          { signal       :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
          , updatedQueue :: IxQueue (read :: READ) (Effects eff) Unit
          }
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
  , toRegister
  , email
  , password
  , submit
  , pendingSignal
  , loginDialogOutputQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        liftEff $ IxSignal.set false pendingSignal
        void $ T.cotransform _ { open = false }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff $ do
          IxSignal.set (Left "") email.signal
          IxSignal.set "" password.signal
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      ClickedRegister -> do
        liftEff toRegister
        performAction Close props state
      SubmitLogin -> do
        mEmail <- liftEff $ do
          IxSignal.set true pendingSignal
          IxSignal.get email.signal
        case mEmail of
          Right (Just email) -> do
            liftBase $ do
              password <- liftEff (IxSignal.get password.signal)
              hashedPassword <- hashPassword {salt: env.salt, password}
              liftEff $ One.putQueue loginDialogOutputQueue {email,password: hashedPassword}
          _ -> liftEff $ log "bad email!" -- FIXME bug out somehow?

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
            [ dialogTitle {} [R.text "Login"]
            , dialogContent {style: createStyles {position: "relative"}}
              [ Email.email
                { label: R.text "Email"
                , fullWidth: true
                , name: "login-email"
                , id: "login-email"
                , emailSignal: email.signal
                , parentSignal: Nothing
                , updatedQueue: email.updatedQueue
                }
              , Password.password
                { label: R.text "Password"
                , fullWidth: true
                , name: "login-password"
                , id: "login-password"
                , passwordSignal: password.signal
                , parentSignal: Nothing
                , updatedQueue: password.updatedQueue
                , errorQueue: password.errorQueue
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
                           { origin: state.currentPage
                           }
                         }
                      , mkFab "#1da1f3" "#0f8cdb" twitterIcon Nothing
                      , mkFab "#dd4e40" "#c13627" googleIcon Nothing
                      ]
              , pending
                { pendingSignal
                }
              ]
            , dialogActions {}
              [ button
                { color: Button.secondary
                , onClick: mkEffFn1 preventDefault
                , onTouchTap: mkEffFn1 \e -> do
                    preventDefault e
                    dispatch ClickedRegister
                , href: URI.print $ toURI $ toLocation $ registerLink :: siteLinks
                } [R.text "Register"]
              , Submit.submit
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



loginDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { loginDialogQueue  :: OneIO.IOQueues (Effects eff) Unit {email :: EmailAddress, password :: HashedPassword}
               , returnLoginQueue  :: One.Queue (write :: WRITE) (Effects eff) (Maybe Unit)
               , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
               , currentPageSignal :: IxSignal (Effects eff) siteLinks
               , toURI             :: Location -> URI
               , env               :: Env
               , toRegister        :: Eff (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog
  { loginDialogQueue: OneIO.IOQueues {input: loginDialogInputQueue, output: loginDialogOutputQueue}
  , returnLoginQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  , toRegister
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
            , toRegister
            , email:
              { signal: emailSignal
              , updatedQueue: emailQueue
              }
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
            , loginDialogOutputQueue
            } )
          (initialState init)
      reactSpecLogin =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Queue.whileMountedOne
            loginDialogInputQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
        $ Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitLogin)
        $ Queue.whileMountedOne
            (One.allowReading returnLoginQueue)
            (\this mErr -> case mErr of
                Nothing -> unsafeCoerceEff $ dispatcher this Close
                Just _ -> One.putQueue passwordErrorQueue unit
                )
        -- $ Queue.whileMountedOne
        --     openLoginSignal
        --     (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
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
            case mEmail of
              Right (Just _) -> do
                p1 <- IxSignal.get passwordSignal
                IxSignal.set (p1 == "") submitDisabledSignal
              _ -> IxSignal.set true submitDisabledSignal
      IxQueue.onIxQueue emailQueue k \_ -> submitValue
      IxQueue.onIxQueue passwordQueue k \_ -> submitValue
      IxSignal.subscribe (\_ -> submitValue) passwordSignal
