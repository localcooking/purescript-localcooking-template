module LocalCooking.Spec where

import LocalCooking.Spec.Topbar (topbar)
import LocalCooking.Spec.Content.Register (register)
import LocalCooking.Spec.Content.UserDetails.Security (security)
import LocalCooking.Spec.Dialogs.Login (loginDialog)
import LocalCooking.Spec.Drawers.LeftMenu (leftMenu)
import LocalCooking.Spec.Snackbar (messages, SnackbarMessage (..), UserEmailError (..))
import LocalCooking.Spec.Flags.USA (usaFlag, usaFlagViewBox)
import LocalCooking.Spec.Flags.Colorado (coloradoFlag, coloradoFlagViewBox)
import LocalCooking.Window (WindowSize (Laptop))
import LocalCooking.Types.Env (Env)
import LocalCooking.Links.Class (registerLink, rootLink, userDetailsLink, getUserDetailsLink, userDetailsGeneralLink, userDetailsSecurityLink, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Auth.Error (AuthError (AuthExistsFailure), PreliminaryAuthToken (..))
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Client.Dependencies.AuthToken
  ( AuthTokenSparrowClientQueues
  , AuthTokenInitIn (..), AuthTokenInitOut (..), AuthTokenDeltaIn (..), AuthTokenDeltaOut (..)
  )
import LocalCooking.Client.Dependencies.Register (RegisterSparrowClientQueues)
import LocalCooking.Client.Dependencies.UserEmail (UserEmailSparrowClientQueues, UserEmailInitIn (..), UserEmailInitOut (..))

import Sparrow.Client.Queue (callSparrowClientQueues)

import Prelude
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Text.Email.Validate (EmailAddress)
import Control.Monad.Aff (makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Base (liftBase)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.SVG as RS
import React.Signal.WhileMounted as Signal
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

import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State siteLinks =
  { authToken :: Maybe AuthToken
  , currentPage :: siteLinks
  , windowSize :: WindowSize
  }


initialState :: forall siteLinks
              . {initSiteLinks :: siteLinks, initWindowSize :: WindowSize} -> State siteLinks
initialState {initSiteLinks,initWindowSize} =
  { authToken: Nothing
  , currentPage: initSiteLinks
  , windowSize: initWindowSize
  }


data Action siteLinks
  = GotAuthToken (Maybe AuthToken)
  | CallAuthToken AuthTokenInitIn
  | ChangedCurrentPage siteLinks
  | ChangedWindowSize WindowSize
  | Logout


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

spec :: forall eff siteLinks userDetailsLinks
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => ToLocation siteLinks
     => { toURI              :: Location -> URI
        , windowSizeSignal   :: IxSignal (Effects eff) WindowSize
        , currentPageSignal  :: IxSignal (Effects eff) siteLinks
        , siteLinks          :: siteLinks -> Eff (Effects eff) Unit
        , development        :: Boolean
        , authTokenQueues    :: AuthTokenSparrowClientQueues (Effects eff)
        , registerQueues     :: RegisterSparrowClientQueues (Effects eff)
        , userEmailQueues      :: UserEmailSparrowClientQueues (Effects eff)
        , errorMessageQueue  :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
        , loginPendingSignal :: One.Queue (read :: READ, write :: WRITE) (Effects eff) Unit
        , authTokenSignal    :: IxSignal (Effects eff) (Maybe AuthToken)
        , userEmailSignal    :: IxSignal (Effects eff) (Maybe EmailAddress)
        , templateArgs ::
          { content :: { toURI :: Location -> URI
                       , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                       , currentPageSignal :: IxSignal (Effects eff) siteLinks
                       , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                       , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                       , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                       } -> Array R.ReactElement
          , topbar ::
            { imageSrc :: Location
            , buttons :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            }
          , leftDrawer ::
            { buttons :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            }
          , userDetails ::
            { buttons :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            , content :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          }
        , env :: Env
        , extendedNetwork :: Array R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { toURI
  , windowSizeSignal
  , siteLinks
  , currentPageSignal
  , development
  , authTokenQueues: authTokenQueues@{deltaIn: authTokenQueuesDeltaIn}
  , registerQueues
  , userEmailQueues
  , errorMessageQueue
  , loginPendingSignal
  , authTokenSignal
  , userEmailSignal
  , templateArgs: templateArgs@{palette,content,userDetails}
  , env
  , extendedNetwork
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p -> void $ T.cotransform _ { currentPage = p }
      ChangedWindowSize p -> void $ T.cotransform _ { windowSize = p }
      Logout -> liftEff $ One.putQueue authTokenQueuesDeltaIn AuthTokenDeltaInLogout
      -- Mapping between programmatic authToken signal and UI shared state & error signaling
      GotAuthToken mToken -> void $ T.cotransform _ { authToken = mToken }
      CallAuthToken initIn -> do
        let onDeltaOut deltaOut = case deltaOut of
              AuthTokenDeltaOutRevoked -> IxSignal.set Nothing authTokenSignal -- TODO verify this is enough to trigger a complete remote logout
              AuthTokenDeltaOutNew authToken' -> pure unit -- FIXME TODO
        mInitOut <- liftBase $ callSparrowClientQueues authTokenQueues onDeltaOut initIn
        liftEff $ do
          case mInitOut of
            Nothing -> do
              IxSignal.set Nothing authTokenSignal
              One.putQueue errorMessageQueue (SnackbarMessageAuthError AuthExistsFailure)
            Just {initOut,deltaIn: _,unsubscribe} -> case initOut of
              AuthTokenInitOutSuccess authToken -> do
                IxSignal.set (Just authToken) authTokenSignal
                -- fetch user email
                case initIn of
                  AuthTokenInitInLogin {email} ->
                    IxSignal.set (Just email) userEmailSignal
                  AuthTokenInitInExists _ ->
                    OneIO.callAsyncEff userEmailQueues
                      (\mInitOut -> case mInitOut of
                          Nothing ->
                            One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoInitOut)
                          Just initOut -> case initOut of
                            UserEmailInitOutSuccess email ->
                              IxSignal.set (Just email) userEmailSignal
                            UserEmailInitOutNoAuth ->
                              One.putQueue errorMessageQueue (SnackbarMessageUserEmail UserEmailNoAuth)
                      )
                      (UserEmailInitIn authToken)
              AuthTokenInitOutFailure e -> do
                unsubscribe
                IxSignal.set Nothing authTokenSignal
                One.putQueue errorMessageQueue (SnackbarMessageAuthFailure e)
          One.putQueue loginPendingSignal unit


    render :: T.Render (State siteLinks) Unit (Action siteLinks)
    render dispatch props state children = template $
      [ topbar
        { toURI
        , openLoginSignal: One.writeOnly openLoginSignal
        , windowSizeSignal
        , siteLinks
        , mobileMenuButtonSignal: One.writeOnly mobileMenuButtonSignal
        , currentPageSignal
        , authTokenSignal
        , userEmailSignal
        , imageSrc: templateArgs.topbar.imageSrc
        , buttons: templateArgs.topbar.buttons
        }
      ] <> mainContent <>
      [ loginDialog
        { openLoginSignal: One.readOnly openLoginSignal
        , windowSizeSignal
        , toURI
        , currentPageSignal
        , login: \email password -> makeAff \resolve -> do
            unsafeCoerceEff $ dispatch $ CallAuthToken $ AuthTokenInitInLogin {email,password}
            One.onQueue loginPendingSignal \_ -> resolve (Right unit)
            pure nonCanceler
        , toRegister: siteLinks registerLink
        , env
        }
      , leftMenu
        { mobileDrawerOpenSignal: One.readOnly mobileMenuButtonSignal
        , siteLinks
        , toURI
        , windowSizeSignal
        , authTokenSignal
        , currentPageSignal
        , userEmailSignal
        , buttons: templateArgs.leftDrawer.buttons
        }
      , messages
        { errorMessageQueue: One.readOnly errorMessageQueue
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

        openLoginSignal :: One.Queue (read :: READ, write :: WRITE) (Effects eff) Unit
        openLoginSignal = unsafePerformEff One.newQueue

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
                  [ Drawer.withStyles
                    (\_ -> {paper: createStyles {position: "relative", width: "200px", zIndex: 1000}})
                    \{classes} -> drawer
                      { variant: Drawer.permanent
                      , anchor: Drawer.left
                      , classes: Drawer.createClasses classes
                      }
                      [ list {dense: true} $
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
                        ] <> userDetails.buttons
                              { siteLinks
                              , currentPageSignal
                              , windowSizeSignal
                              , authTokenSignal
                              , userEmailSignal
                              , toURI
                              }
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
                  , R.div [RP.style {position: "absolute", left: "230px", top: "1em"}] $
                    -- TODO pack currentPageSignal listener to this level, so side buttons
                    -- aren't redrawn
                    let  def =
                          userDetails.content
                            { siteLinks
                            , currentPageSignal
                            , windowSizeSignal
                            , authTokenSignal
                            , userEmailSignal
                            , toURI
                            }
                    in  case mUserDetails of
                      Just d
                        | d == userDetailsSecurityLink ->
                          [security]
                        | otherwise -> def
                      _ -> def
                  ]

                _ | state.currentPage == registerLink ->
                      [ register
                        { registerQueues
                        , errorMessageQueue: One.writeOnly errorMessageQueue
                        , toRoot: siteLinks rootLink
                        , env
                        }
                      ]
                  | otherwise ->
                      content
                        { siteLinks
                        , currentPageSignal
                        , windowSizeSignal
                        , authTokenSignal
                        , userEmailSignal
                        , toURI
                        }
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
          , R.div [RP.style {textAlign: "center"}]
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



app :: forall eff siteLinks userDetailsLinks
     . LocalCookingSiteLinks siteLinks userDetailsLinks
    => Eq siteLinks
    => ToLocation siteLinks
    => { toURI                :: Location -> URI
       , windowSizeSignal     :: IxSignal (Effects eff) WindowSize
       , currentPageSignal    :: IxSignal (Effects eff) siteLinks
       , siteLinks            :: siteLinks -> Eff (Effects eff) Unit
       , development          :: Boolean
       , preliminaryAuthToken :: PreliminaryAuthToken
       , errorMessageQueue    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
       , authTokenSignal      :: IxSignal (Effects eff) (Maybe AuthToken)
       , authTokenQueues      :: AuthTokenSparrowClientQueues (Effects eff)
       , registerQueues       :: RegisterSparrowClientQueues (Effects eff)
       , userEmailQueues      :: UserEmailSparrowClientQueues (Effects eff)
       , templateArgs ::
          { content :: { toURI :: Location -> URI
                       , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                       , currentPageSignal :: IxSignal (Effects eff) siteLinks
                       , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                       , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                       , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                       } -> Array R.ReactElement
          , topbar ::
            { imageSrc :: Location
            , buttons :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            }
          , leftDrawer ::
            { buttons :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            }
          , userDetails ::
            { buttons :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            , content :: { toURI :: Location -> URI
                          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                          , currentPageSignal :: IxSignal (Effects eff) siteLinks
                          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                          , userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
                          } -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          }
       , env :: Env
       , extendedNetwork :: Array R.ReactElement
       }
    -> { spec :: R.ReactSpec Unit (State siteLinks) (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit (State siteLinks) -> (Action siteLinks) -> T.EventHandler
       }
app
  { toURI
  , windowSizeSignal
  , currentPageSignal
  , siteLinks
  , development
  , preliminaryAuthToken
  , errorMessageQueue
  , authTokenSignal
  , authTokenQueues
  , registerQueues
  , userEmailQueues
  , templateArgs
  , env
  , extendedNetwork
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
        ( spec
          { toURI
          , windowSizeSignal
          , currentPageSignal
          , siteLinks
          , development
          , errorMessageQueue
          , authTokenQueues
          , registerQueues
          , userEmailQueues
          , loginPendingSignal
          , authTokenSignal
          , userEmailSignal
          , templateArgs
          , env
          , extendedNetwork
          }
        ) (initialState init)
      reactSpec' = Signal.whileMountedIxUUID
                     authTokenSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (GotAuthToken x))
                 $ Signal.whileMountedIxUUID
                     currentPageSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
                 $ Signal.whileMountedIxUUID
                     windowSizeSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
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
                unsafeCoerceEff $ One.putQueue errorMessageQueue $ SnackbarMessageAuthError e
          reactSpec.componentWillMount this
        }

  in  {spec: reactSpec', dispatcher}
  where
    loginPendingSignal :: One.Queue (read :: READ, write :: WRITE) (Effects eff) Unit
    loginPendingSignal = unsafePerformEff One.newQueue

    userEmailSignal :: IxSignal (Effects eff) (Maybe EmailAddress)
    userEmailSignal = unsafePerformEff (IxSignal.make Nothing)
