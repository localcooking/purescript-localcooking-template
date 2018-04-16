module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Spec.Content.Register.Pending (pending)
import LocalCooking.Spec.Form.Email (email)
import LocalCooking.Spec.Form.Password (password)
import LocalCooking.Spec.Form.Submit (submit)
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
import Text.Email.Validate (EmailAddress, emailAddress)
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
import MaterialUI.Input as Input
import MaterialUI.TextField (textField)
import MaterialUI.CircularProgress (circularProgress)
import Crypto.Scrypt (SCRYPT)

import Queue.One (READ, Queue)
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Unsafe.Coerce (unsafeCoerce)



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
        , login :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
        , toRegister :: Eff (Effects eff) Unit
        , env :: Env
        , emailSignal :: IxSignal (Effects eff) (Either String (Maybe EmailAddress))
        , passwordSignal :: IxSignal (Effects eff) String
        , pendingSignal :: IxSignal (Effects eff) Boolean
        , submitDisabledSignal :: IxSignal (Effects eff) Boolean
        , emailQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , passwordQueue :: IxQueue (read :: READ) (Effects eff) Unit
        , submitQueue :: IxQueue (read :: READ) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { toURI
  , login
  , toRegister
  , env
  , emailSignal
  , passwordSignal
  , pendingSignal
  , submitDisabledSignal
  , emailQueue
  , passwordQueue
  , submitQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        liftEff $ IxSignal.set false pendingSignal
        void $ T.cotransform _ { open = false }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff $ do
          IxSignal.set (Left "") emailSignal
          IxSignal.set "" passwordSignal
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      ClickedRegister -> do
        liftEff toRegister
        performAction Close props state
      SubmitLogin -> do
        mEmail <- liftEff $ do
          IxSignal.set true pendingSignal
          IxSignal.get emailSignal
        case mEmail of
          Right (Just email) -> do
            liftBase $ do
              password <- liftEff (IxSignal.get passwordSignal)
              hashedPassword <- hashPassword {salt: env.salt, password}
              login email hashedPassword
            performAction Close props state
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
              [ email
                { label: R.text "Email"
                , fullWidth: true
                , name: "login-email"
                , id: "login-email"
                , emailSignal
                , parentSignal: Nothing
                , updatedQueue: emailQueue
                }
              , password
                { label: R.text "Password"
                , fullWidth: true
                , name: "login-password"
                , id: "login-password"
                , passwordSignal
                , parentSignal: Nothing
                , updatedQueue: passwordQueue
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
              , submit
                { color: Button.primary
                , variant: Button.flat
                , size: Button.medium
                , style: createStyles {}
                , triggerQueue: submitQueue
                , disabledSignal: submitDisabledSignal
                } [R.text "Submit"]
                -- button
                -- { color: Button.primary
                -- , disabled: case emailAddress state.email of
                --   Nothing -> state.password == ""
                --   Just _ -> false
                -- , onTouchTap: mkEffFn1 \_ -> dispatch SubmitLogin
                -- } [R.text "Submit"]
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
            => { openLoginSignal   :: Queue (read :: READ) (Effects eff) Unit
               , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
               , toURI             :: Location -> URI
               , currentPageSignal :: IxSignal (Effects eff) siteLinks
               , login             :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
               , toRegister        :: Eff (Effects eff) Unit
               , env               :: Env
               }
            -> R.ReactElement
loginDialog
  { openLoginSignal
  , windowSizeSignal
  , toURI
  , currentPageSignal
  , login
  , toRegister
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
            , login
            , toRegister
            , env
            , emailSignal
            , passwordSignal
            , submitDisabledSignal
            , emailQueue
            , passwordQueue
            , pendingSignal
            , submitQueue
            } )
          (initialState init)
      reactSpecLogin =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this SubmitLogin)
        -- FIXME listen on authTokenSignal
        -- $ Signal.whileMountedIxUUID
        --     userDetailsSignal
        --     (\this x -> unsafeCoerceEff $ case x of
        --         Nothing -> pure unit
        --         Just _ -> dispatcher this Close)
        $ Queue.whileMountedOne
            openLoginSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Left ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    submitDisabledSignal = unsafePerformEff $ IxSignal.make false
    emailQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ IxQueue.readOnly <$> IxQueue.newIxQueue
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
