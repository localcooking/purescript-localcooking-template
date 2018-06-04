module LocalCooking.Spec.Content where

import LocalCooking.Spec.Content.UserDetails.Security (security)
import LocalCooking.Spec.Content.Register (register)
import LocalCooking.Spec.Flags.USA (usaFlag, usaFlagViewBox)
import LocalCooking.Spec.Flags.Colorado (coloradoFlag, coloradoFlagViewBox)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingState, initLocalCookingState, performActionLocalCooking, LocalCookingAction, whileMountedLocalCooking)
import LocalCooking.Spec.Snackbar (SnackbarMessage (..))
import LocalCooking.Dependencies (Queues)
import LocalCooking.Dependencies.AuthToken (AuthTokenInitIn, AuthTokenDeltaIn)
import LocalCooking.User.Class (class UserDetails)
import LocalCooking.Links.Class (class LocalCookingSiteLinks, getUserDetailsLink, userDetailsLink, userDetailsGeneralLink, userDetailsSecurityLink, registerLink, rootLink)
import Facebook.State (FacebookLoginUnsavedFormData)

import Prelude
import Data.URI.Location (Location, class ToLocation)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Lens (Lens', Prism', lens, prism')
import Data.Generic (class Generic)
import Text.Email.Validate (EmailAddress)
import Control.Monad.Eff (Eff)
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
import DOM.HTML.Window.Extra (WindowSize (Laptop))
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


type Action siteLinks userDetails = LocalCookingAction siteLinks userDetails


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
getLCAction = prism' id Just



spec :: forall eff siteLinks userDetailsLinks userDetails siteQueues
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
        , dependenciesQueues  :: Queues siteQueues (Effects eff)
        , authTokenInitIn :: AuthTokenInitIn -> Eff (Effects eff) Unit
        , authTokenDeltaIn :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
        , dialog ::
          { authenticateQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
          , privacyPolicyQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
          }
        , templateArgs ::
          { content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          -- , topbar ::
          --   { imageSrc :: Location
          --   , buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          --   }
          -- , leftDrawer ::
          --   { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
          --   }
          , userDetails ::
            { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            , content :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
          , palette :: {primary :: ColorPalette, secondary :: ColorPalette}
          , initFormDataRef :: Ref (Maybe FacebookLoginUnsavedFormData)
          }
        , extendedNetwork :: Array R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params@{siteLinks,authTokenSignal}
  { development
  , errorMessageQueue
  , dependenciesQueues  -- : dependencies@{authTokenQueues:{deltaIn: authTokenQueuesDeltaIn}}
  , authTokenInitIn
  , authTokenDeltaIn
  , dialog
  , templateArgs: templateArgs@{palette,content,userDetails}
  , env
  , initFormDataRef
  , extendedNetwork
  } = T.simpleSpec performAction render
  where
    performAction = performActionLocalCooking getLCState

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children =
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
              -- FIXME User details component
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
                      ] <> userDetails.buttons params <>
                      [ listItem
                        { button: true
                        , onClick: mkEffFn1 \_ -> pure unit -- dispatch Logout
                          -- FIXME goddamnit.    
                          -- FIXME feels weird - shouldn't this be its own sidebar component?
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
                        , setUserQueues: dependenciesQueues.commonQueues.setUserQueues
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
                    { registerQueues: dependenciesQueues.commonQueues.registerQueues
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
      , -- FIXME footer component? Nah, just pack in content
        typography
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



content :: forall eff siteLinks userDetailsLinks userDetails siteQueues
         . LocalCookingSiteLinks siteLinks userDetailsLinks
        => Eq siteLinks
        => ToLocation siteLinks
        => UserDetails userDetails
        => Generic siteLinks
        => Generic userDetails
        => LocalCookingParams siteLinks userDetails (Effects eff)
        -> { development          :: Boolean
           , env                  :: Env
           , errorMessageQueue    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) SnackbarMessage
           -- FIXME TODO restrict authTokenQueues from being visible
           , dependenciesQueues   :: Queues siteQueues (Effects eff)
           , authTokenInitIn      :: AuthTokenInitIn -> Eff (Effects eff) Unit
           , authTokenDeltaIn     :: AuthTokenDeltaIn -> Eff (Effects eff) Unit
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
           , initFormDataRef :: Ref (Maybe FacebookLoginUnsavedFormData)
           , extendedNetwork :: Array R.ReactElement
           }
        -> R.ReactElement
content
  params
  { development
  , errorMessageQueue
  , dependenciesQueues
  , authTokenInitIn
  , authTokenDeltaIn
  , templateArgs
  , env
  , initFormDataRef
  , extendedNetwork
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
        ( spec
          params
          { development
          , errorMessageQueue
          , dependenciesQueues
          , authTokenInitIn
          , authTokenDeltaIn
          , dialog:
            { authenticateQueue: authenticateDialogQueue
            }
          , templateArgs
          , env
          , initFormDataRef
          , extendedNetwork
          }
        ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "LocalCooking.Spec"
            id
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit [] -- {spec: reactSpec', dispatcher}
  where
    -- FIXME dialog queues - would there be any spawned by a dep's async incoming?

    authenticateDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe HashedPassword)
    authenticateDialogQueue = unsafePerformEff OneIO.newIOQueues
