module LocalCooking.Spec.Dialogs.Login where

import LocalCooking.Types.Env (Env)

import LocalCooking.Window (WindowSize (..))
import LocalCooking.Links (ThirdPartyLoginReturnLinks (..))
import LocalCooking.Links.Class (registerLink, toLocation, class LocalCookingSiteLinks, class ToLocation)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..))
import LocalCooking.Common.Password (HashedPassword, hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
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
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Unsafe.Coerce (unsafeCoerce)



type State siteLinks =
  { open :: Boolean
  , windowSize :: WindowSize
  , currentPage :: siteLinks
  , email :: String
  , emailDirty :: Maybe Boolean
  , password :: String
  , pending :: Boolean
  }


initialState :: forall siteLinks
              . {initSiteLinks :: siteLinks, initWindowSize :: WindowSize} -> State siteLinks
initialState {initWindowSize, initSiteLinks} =
  { open: false
  , windowSize: initWindowSize
  , currentPage: initSiteLinks
  , email: ""
  , emailDirty: Nothing
  , password: ""
  , pending: false
  }


data Action siteLinks
  = Open
  | Close
  | ChangedWindowSize WindowSize
  | ChangedPage siteLinks
  | ChangedEmail String
  | EmailUnfocused
  | ChangedPassword String
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


spec :: forall eff siteLinks
      . LocalCookingSiteLinks siteLinks
     => ToLocation siteLinks
     => { toURI :: Location -> URI
        , login :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
        , toRegister :: Eff (Effects eff) Unit
        , env :: Env
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec {toURI,login,toRegister,env} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        void $ T.cotransform _ { open = false, pending = false }
        liftBase $ delay $ Milliseconds 2000.0
        void $ T.cotransform _ { email = "", password = "" }
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      ChangedEmail e -> void $ T.cotransform _ { email = e, emailDirty = Just false }
      ClickedRegister -> do
        liftEff toRegister
        performAction Close props state
      EmailUnfocused -> void $ T.cotransform _ { emailDirty = Just true }
      ChangedPassword p -> void $ T.cotransform _ { password = p }
      SubmitLogin -> do
        void $ T.cotransform _ { pending = true }
        case emailAddress state.email of
          Nothing -> liftEff $ log "bad email!" -- FIXME bug out somehow?
          Just email -> do
            liftBase $ do
              hashedPassword <- hashPassword {salt: env.salt, password: state.password}
              login email hashedPassword
            performAction Close props state
            -- liftEff $ replaceState' state.currentPage -- FIXME weird password triggering

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
                  , onClose: mkEffFn1 \_ ->
                      when (not state.pending) (dispatch Close)
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Login"]
            , dialogContent {style: createStyles {position: "relative"}}
              [ textField
                { label: R.text "Email"
                , fullWidth: true
                , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
                , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
                , error: case emailAddress state.email of
                  Nothing -> state.emailDirty == Just true
                  Just _ -> false
                , name: "login-email"
                , id: "login-email"
                }
              , textField
                { label: R.text "Password"
                , fullWidth: true
                , "type": Input.passwordType
                , onChange: mkEffFn1 \p -> dispatch $ ChangedPassword (unsafeCoerce p).target.value
                , name: "login-password"
                , id: "login-password"
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
            , dialogActions {}
              [ button
                { color: Button.secondary
                , onClick: mkEffFn1 preventDefault
                , onTouchTap: mkEffFn1 \e -> do
                    preventDefault e
                    dispatch ClickedRegister
                , href: URI.print $ toURI $ toLocation $ registerLink :: siteLinks
                } [R.text "Register"]
              , button
                { color: Button.primary
                , disabled: case emailAddress state.email of
                  Nothing -> state.password == ""
                  Just _ -> false
                , onTouchTap: mkEffFn1 \_ -> dispatch SubmitLogin
                } [R.text "Submit"]
              , button
                { color: Button.default
                , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Cancel"]
              ]
            ]
      ]



loginDialog :: forall eff siteLinks
             . LocalCookingSiteLinks siteLinks
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
          (spec {toURI,login,toRegister,env})
          (initialState init)
      reactSpecLogin =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
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
