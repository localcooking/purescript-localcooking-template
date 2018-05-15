module LocalCooking.Spec where

import LocalCooking.Spec.Topbar (topbar)
import LocalCooking.Spec.Content.Register (register)
import LocalCooking.Spec.Content.UserDetails.Security (security)
import LocalCooking.Spec.Dialogs.Login (loginDialog)
import LocalCooking.Spec.Dialogs.Authenticate (authenticateDialog)
import LocalCooking.Spec.Dialogs.PrivacyPolicy (privacyPolicyDialog)
import LocalCooking.Spec.Drawers.LeftMenu (leftMenu)
import LocalCooking.Spec.Snackbar (messages, SnackbarMessage (..), RedirectError (RedirectLogout))
import LocalCooking.Spec.Flags.USA (usaFlag, usaFlagViewBox)
import LocalCooking.Spec.Flags.Colorado (coloradoFlag, coloradoFlagViewBox)
import LocalCooking.Window (WindowSize (Laptop))
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)
import LocalCooking.Links.Class (registerLink, rootLink, userDetailsLink, getUserDetailsLink, userDetailsGeneralLink, userDetailsSecurityLink, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Client.Dependencies.AuthToken
  ( AuthTokenSparrowClientQueues
  , AuthTokenInitIn (..), AuthTokenInitOut (..), AuthTokenDeltaIn (..), AuthTokenDeltaOut (..)
  , AuthTokenFailure (AuthExistsFailure), PreliminaryAuthToken (..))
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues)
import LocalCooking.Client.Dependencies.UserEmail (UserEmailSparrowClientQueues)
import LocalCooking.Client.Dependencies.Security (SecuritySparrowClientQueues)
import LocalCooking.Client.Dependencies.PasswordVerify (PasswordVerifySparrowClientQueues)
import LocalCooking.User (class UserDetails)
import Facebook.State (FacebookLoginUnsavedFormData)

import Sparrow.Client.Queue (callSparrowClientQueues)

import Prelude
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Lens (Lens', Prism', lens, prism')
import Data.Generic (class Generic)
import Text.Email.Validate (EmailAddress)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Base (liftBase)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.SVG as RS
import MaterialUI.MuiThemeProvider (ColorPalette, muiThemeProvider, createMuiTheme)
import MaterialUI.CssBaseline (cssBaseline)
import MaterialUI.Paper (paper)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.Types (createStyles)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)

import Queue.Types (writeOnly, readOnly)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxSignal.Internal as IxSignal



type State siteLinks userDetails = LocalCookingState siteLinks userDetails


initialState :: forall siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState = id


data Action siteLinks userDetails
  = Logout
  | AttemptLogin
  | CallAuthToken AuthTokenInitIn
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)


type Effects eff =
  ( ref        :: REF
  , exception  :: EXCEPTION
  , uuid       :: GENUUID
  , dom        :: DOM
  , history    :: HISTORY
  , now        :: NOW
  , timer      :: TIMER
  , webStorage :: WEB_STORAGE
  , console    :: CONSOLE
  , scrypt     :: SCRYPT
  | eff)

getLCState :: forall siteLinks userDetails. Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens id (\_ x -> x)

getLCAction :: forall siteLinks userDetails. Prism' (Action siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' LocalCookingAction $ case _ of
  LocalCookingAction x -> Just x
  _ -> Nothing


spec :: forall eff siteLinks userDetailsLinks userDetails
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => ToLocation siteLinks
     => UserDetails userDetails
     => Generic siteLinks
     => Generic userDetails
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { env                 :: Env
        , development         :: Boolean
        , errorMessageQueue   :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
        , initFormDataRef     :: Ref (Maybe FacebookLoginUnsavedFormData)
        , dependencies ::
          { authTokenQueues      :: AuthTokenSparrowClientQueues (Effects eff)
          , registerQueues       :: RegisterSparrowClientQueues (Effects eff)
          , userEmailQueues      :: UserEmailSparrowClientQueues (Effects eff)
          , securityQueues       :: SecuritySparrowClientQueues (Effects eff)
          , passwordVerifyQueues :: PasswordVerifySparrowClientQueues (Effects eff)
          }
        , dialog ::
          { loginQueue         :: OneIO.IOQueues (Effects eff) Unit (Maybe {email :: EmailAddress, password :: HashedPassword})
          , loginCloseQueue    :: One.Queue (write :: WRITE) (Effects eff) Unit
          , authenticateQueue  :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
          , privacyPolicyQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
          }
        , templateArgs ::
          { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          , topbar ::
            { imageSrc :: Location
            , buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , leftDrawer ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , userDetails ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          }
        , extendedNetwork :: Array R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params@{siteLinks,authTokenSignal}
  { development
  , errorMessageQueue
  , dependencies: dependencies@{authTokenQueues:{deltaIn: authTokenQueuesDeltaIn}}
  , dialog
  , templateArgs: templateArgs@{palette,content,userDetails}
  , env
  , extendedNetwork
  , initFormDataRef
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Logout -> liftEff $ do
        One.putQueue authTokenQueuesDeltaIn AuthTokenDeltaInLogout
        void $ setTimeout 500 $
          One.putQueue errorMessageQueue $ SnackbarMessageRedirect RedirectLogout
        -- siteLinks rootLink
        IxSignal.set Nothing authTokenSignal
      AttemptLogin -> do
        mEmailPassword <- liftBase (OneIO.callAsync dialog.loginQueue unit)
        case mEmailPassword of
          Nothing -> pure unit
          Just {email,password} -> do
            let initIn = AuthTokenInitInLogin {email,password}
            performAction (CallAuthToken initIn) props state
      CallAuthToken initIn -> do
        let onDeltaOut deltaOut = case deltaOut of
              AuthTokenDeltaOutRevoked -> IxSignal.set Nothing authTokenSignal
        mInitOut <- liftBase (callSparrowClientQueues dependencies.authTokenQueues onDeltaOut initIn)
        liftEff $ do
          case mInitOut of
            Nothing -> do
              IxSignal.set Nothing authTokenSignal
              One.putQueue errorMessageQueue (SnackbarMessageAuthFailure AuthExistsFailure)
            Just {initOut,deltaIn: _,unsubscribe} -> case initOut of
              AuthTokenInitOutSuccess authToken -> do
                IxSignal.set (Just authToken) authTokenSignal
              AuthTokenInitOutFailure e -> do
                unsubscribe
                IxSignal.set Nothing authTokenSignal
                One.putQueue errorMessageQueue (SnackbarMessageAuthFailure e)
      LocalCookingAction a -> do
        liftEff $ log $ "Received message...? " <> show a
        performActionLocalCooking getLCState a props state


    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children = template $
      [ topbar
        params
        { openLogin: unsafeCoerceEff $ dispatch AttemptLogin
        , mobileMenuButtonSignal: writeOnly mobileMenuButtonSignal
        , imageSrc: templateArgs.topbar.imageSrc
        , buttons: templateArgs.topbar.buttons
        }
      ] <> mainContent <>
      [ loginDialog
        params
        { loginDialogQueue: dialog.loginQueue
        , loginCloseQueue: dialog.loginCloseQueue
        , passwordVerifyQueues: dependencies.passwordVerifyQueues
        , errorMessageQueue: writeOnly errorMessageQueue
        , toRegister: siteLinks registerLink
        , env
        }
      , authenticateDialog
        params
        { authenticateDialogQueue: dialog.authenticateQueue
        , passwordVerifyQueues: dependencies.passwordVerifyQueues
        , errorMessageQueue: writeOnly errorMessageQueue
        , env
        }
      , privacyPolicyDialog
        params
        { privacyPolicyDialogQueue: dialog.privacyPolicyQueue
        }
      , leftMenu
        params
        { mobileDrawerOpenSignal: readOnly mobileMenuButtonSignal
        , buttons: templateArgs.leftDrawer.buttons
        }
      , messages
        { errorMessageQueue: readOnly errorMessageQueue
        }
      ]
      where
        template xs =
          [ cssBaseline
          , muiThemeProvider
              { theme: createMuiTheme {palette}
              }
              (R.div [] xs)
          ]

        mobileMenuButtonSignal :: One.Queue (read :: READ, write :: WRITE) (Effects eff) Unit
        mobileMenuButtonSignal = unsafePerformEff One.newQueue

        mainContent :: Array R.ReactElement
        mainContent =
          [ R.main [RP.style {marginTop: "4.5em"}]
            [ paper
              { style: if state.windowSize < Laptop
                          then createStyles
                                  { width: "100%"
                                  , position: "relative"
                                  , minHeight: "30em"
                                  , padding: "1em"
                                  }
                          else createStyles
                                  { maxWidth: "80em"
                                  , width: "100%"
                                  , marginLeft: "auto"
                                  , marginRight: "auto"
                                  , padding: "1em"
                                  , position: "relative"
                                  , minHeight: "30em"
                                  }
              } $ case getUserDetailsLink state.currentPage of
                Just mUserDetails ->
                  -- TODO responsive design for side-drawer navigation
                  [ R.div [RP.style {position: "relative"}]
                    [ Drawer.withStyles
                      (\_ -> {paper: createStyles {position: "relative", width: "200px", zIndex: 1000}})
                      \{classes} -> drawer
                        { variant: Drawer.permanent
                        , anchor: Drawer.left
                        , classes: Drawer.createClasses classes
                        }
                        [ list {} $
                          [ listItem
                            { button: true
                            , onClick: mkEffFn1 \_ -> unsafeCoerceEff
                                                    $ siteLinks $ userDetailsLink
                                                    $ Just userDetailsGeneralLink
                            }
                            [ listItemText
                              { primary: "General"
                              }
                            ]
                          , divider {}
                          , listItem
                            { button: true
                            , onClick: mkEffFn1 \_ -> unsafeCoerceEff
                                                    $ siteLinks $ userDetailsLink
                                                    $ Just userDetailsSecurityLink
                            }
                            [ listItemText
                              { primary: "Security"
                              }
                            ]
                          , divider {}
                          ] <> userDetails.buttons params
                            <>
                          [ listItem
                            { button: true
                            , onClick: mkEffFn1 \_ -> dispatch Logout
                            }
                            [ listItemText
                              { primary: "Logout"
                              }
                            ]
                          ]
                        ]
                      ]
                  , R.div [RP.style {position: "absolute", left: "216px", top: "1em", paddingLeft: "1em"}] $
                    -- TODO pack currentPageSignal listener to this level, so side buttons
                    -- aren't redrawn
                    case mUserDetails of
                      Just d
                        | d == userDetailsSecurityLink ->
                          [ security
                            params
                            { env
                            , errorMessageQueue: writeOnly errorMessageQueue
                            , securityQueues: dependencies.securityQueues
                            , authenticateDialogQueue: dialog.authenticateQueue
                            , initFormDataRef
                            }
                          ]
                        | otherwise -> userDetails.content params
                      _ -> userDetails.content params
                  ]

                _ | state.currentPage == registerLink ->
                      [ register
                        params
                        { registerQueues: dependencies.registerQueues
                        , errorMessageQueue: writeOnly errorMessageQueue
                        , privacyPolicyQueue: dialog.privacyPolicyQueue
                        , toRoot: siteLinks rootLink
                        , env
                        , initFormDataRef
                        }
                      ]
                  | otherwise ->
                      content params
            ]
          , typography
            { variant: Typography.subheading
            , style: createStyles {color: "rgba(255,255,255,0.5)", marginTop: "5em"}
            , align: Typography.center
            }
            [ R.text "Extended Network"]
          , R.div
            [ RP.style {textAlign: "center", marginBottom: "5em"}
            ] extendedNetwork
          , divider {}
          , typography
            { variant: Typography.caption
            , style: createStyles {marginTop: "5em"}
            , align: Typography.center
            }
            [ R.text "Copyright © Local Cooking Inc. 2018, All rights reserved." ]
          , typography
            { variant: Typography.caption
            , align: Typography.center
            }
            [ R.text "Proudly made in Golden, Colorado, The United States of America."
            ]
          , R.div [RP.style {textAlign: "center", marginTop: "1em"}]
            [ RS.svg
                [ RP.viewBox coloradoFlagViewBox
                , RP.width (show flagWidth)
                , RP.height (show flagHeight)
                ] coloradoFlag
            , RS.svg
                [ RP.viewBox usaFlagViewBox
                , RP.width (show flagWidth)
                , RP.height (show flagHeight)
                ] usaFlag
            ]
          ]
          where
            flagWidth = 48
            flagHeight = 26



app :: forall eff siteLinks userDetailsLinks userDetails
     . LocalCookingSiteLinks siteLinks userDetailsLinks
    => Eq siteLinks
    => ToLocation siteLinks
    => UserDetails userDetails
    => Generic siteLinks
    => Generic userDetails
    => LocalCookingParams siteLinks userDetails (Effects eff)
    -> { development          :: Boolean
       , env                  :: Env
       , preliminaryAuthToken :: PreliminaryAuthToken
       , errorMessageQueue    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
       , loginCloseQueue      :: One.Queue (write :: WRITE) (Effects eff) Unit
       , initFormDataRef      :: Ref (Maybe FacebookLoginUnsavedFormData)
       , dependencies ::
          { authTokenQueues      :: AuthTokenSparrowClientQueues (Effects eff)
          , registerQueues       :: RegisterSparrowClientQueues (Effects eff)
          , userEmailQueues      :: UserEmailSparrowClientQueues (Effects eff)
          , securityQueues       :: SecuritySparrowClientQueues (Effects eff)
          , passwordVerifyQueues :: PasswordVerifySparrowClientQueues (Effects eff)
          }
       , templateArgs ::
          { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          , topbar ::
            { imageSrc :: Location
            , buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , leftDrawer ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , userDetails ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          }
       , extendedNetwork :: Array R.ReactElement
       }
    -> { spec :: R.ReactSpec Unit (State siteLinks userDetails) (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit (State siteLinks userDetails) -> (Action siteLinks userDetails) -> T.EventHandler
       }
app
  params
  { development
  , preliminaryAuthToken
  , errorMessageQueue
  , loginCloseQueue
  , dependencies
  , templateArgs
  , env
  , extendedNetwork
  , initFormDataRef
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
        ( spec
          params
          { development
          , errorMessageQueue
          , dependencies
          , dialog:
            { loginQueue: loginDialogQueue
            , loginCloseQueue
            , authenticateQueue: authenticateDialogQueue
            , privacyPolicyQueue: privacyPolicyDialogQueue
            }
          , templateArgs
          , env
          , extendedNetwork
          , initFormDataRef
          }
        ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
          $ reactSpec
            { componentWillMount = \this -> do
              case preliminaryAuthToken of
                PreliminaryAuthToken Nothing -> pure unit
                PreliminaryAuthToken (Just eErr) -> case eErr of
                  -- Call the authToken resource when the spec starts, using the preliminary auth token
                  Right prescribedAuthToken ->
                    unsafeCoerceEff $ dispatcher this $ CallAuthToken $
                      AuthTokenInitInExists {exists: prescribedAuthToken}
                  Left e ->
                    unsafeCoerceEff $ One.putQueue errorMessageQueue $ SnackbarMessageAuthFailure e
              reactSpec.componentWillMount this
            }

  in  {spec: reactSpec', dispatcher}
  where
    loginDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe {email :: EmailAddress, password :: HashedPassword})
    loginDialogQueue = unsafePerformEff OneIO.newIOQueues

    authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
    authenticateDialogQueue = unsafePerformEff OneIO.newIOQueues

    privacyPolicyDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
    privacyPolicyDialogQueue = unsafePerformEff OneIO.newIOQueues
