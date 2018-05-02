module LocalCooking.Spec.Topbar where

import LocalCooking.Links.Class (class LocalCookingSiteLinks, class ToLocation, toLocation, rootLink, getUserDetailsLink, userDetailsLink)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.User (class UserDetails, getEmailAddress)

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.AppBar (appBar)
import MaterialUI.AppBar as AppBar
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.IconButton (iconButton)
import MaterialUI.IconButton as IconButton
import MaterialUI.Icons.Menu (menuIcon)

import Queue.One (WRITE, Queue, putQueue)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State siteLinks userDetails =
  { windowSize :: WindowSize
  , currentPage :: siteLinks
  , authToken :: Maybe AuthToken
  , userDetails :: Maybe userDetails
  }

initialState :: forall siteLinks userDetails
              . { initWindowSize  :: WindowSize
                , initSiteLinks   :: siteLinks
                , initAuthToken   :: Maybe AuthToken
                , initUserDetails :: Maybe userDetails
                } -> State siteLinks userDetails
initialState {initWindowSize,initSiteLinks,initAuthToken,initUserDetails} =
  { windowSize: initWindowSize
  , currentPage: initSiteLinks
  , authToken: initAuthToken
  , userDetails: initUserDetails
  }

data Action siteLinks userDetails
  = OpenLogin
  | ClickedMobileMenuButton
  | ChangedWindowSize WindowSize
  | ChangedCurrentPage siteLinks
  | ChangedAuthToken (Maybe AuthToken)
  | ChangedUserDetails (Maybe userDetails)
  | Clicked siteLinks

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff siteLinks userDetailsLinks userDetails
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => Eq siteLinks
     => ToLocation siteLinks
     => UserDetails userDetails
     => { toURI :: Location -> URI
        , openLogin :: Eff (Effects eff) Unit
        , siteLinks :: siteLinks -> Eff (Effects eff) Unit
        , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
        , currentPageSignal :: IxSignal (Effects eff) siteLinks
        , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
        , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
        , imageSrc :: Location
        , buttons :: { toURI :: Location -> URI
                      , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                      , currentPageSignal :: IxSignal (Effects eff) siteLinks
                      , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                      , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                      , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
                      } -> Array R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  { toURI
  , openLogin
  , siteLinks
  , mobileMenuButtonSignal
  , windowSizeSignal
  , currentPageSignal
  , authTokenSignal
  , userDetailsSignal
  , imageSrc
  , buttons
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff openLogin
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonSignal unit)
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedCurrentPage x -> void $ T.cotransform _ { currentPage = x }
      ChangedAuthToken x -> void $ T.cotransform _ { authToken = x }
      ChangedUserDetails e -> void $ T.cotransform _ { userDetails = e }
      Clicked x -> liftEff (siteLinks x)

    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children =
      [ appBar {color: AppBar.default, position: AppBar.fixed}
        [ toolbar {style: createStyles {display: "flex"}} $
          ( if state.windowSize < Laptop
            then
              [ iconButton
                { color: IconButton.inherit
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedMobileMenuButton
                } menuIcon
              ]
            else
              [ R.img  [ RP.src $ URI.print $ toURI imageSrc
                      , RP.style {height: "2.5em", border: 0}
                      ] []
              , button
                { color: Button.inherit
                , disabled: state.currentPage == rootLink
                , onClick: mkEffFn1 preventDefault
                , onTouchTap: mkEffFn1 \e -> do
                    preventDefault e
                    dispatch $ Clicked $ rootLink :: siteLinks
                , href: URI.print $ toURI $ toLocation $ rootLink :: siteLinks
                , variant: Button.flat
                } [R.text "About"]
              ] <> buttons
                     { siteLinks
                     , toURI
                     , currentPageSignal
                     , windowSizeSignal
                     , authTokenSignal
                     , userDetailsSignal
                     }
          ) <>
          [ R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}] $ case state.userDetails of
               Nothing ->
                [ button
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch OpenLogin
                  } [R.text "Login"]
                ]
               Just userDetails ->
                [ button -- TODO cart iconButton
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch $ Clicked $ userDetailsLink Nothing :: siteLinks
                  , disabled: case getUserDetailsLink state.currentPage of
                    Just _ -> true
                    _ -> false
                  } [R.text $ Email.toString $ getEmailAddress userDetails]
                ]
          ]
        ]
      ]



topbar :: forall eff siteLinks userDetailsLinks userDetails
        . LocalCookingSiteLinks siteLinks userDetailsLinks
       => Eq siteLinks
       => ToLocation siteLinks
       => UserDetails userDetails
       => { toURI :: Location -> URI
          , openLogin :: Eff (Effects eff) Unit
          , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
          , currentPageSignal :: IxSignal (Effects eff) siteLinks
          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
          , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
          , imageSrc :: Location
          , buttons :: { toURI :: Location -> URI
                        , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                        , currentPageSignal :: IxSignal (Effects eff) siteLinks
                        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                        , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                        , userDetailsSignal :: IxSignal (Effects eff) (Maybe userDetails)
                        } -> Array R.ReactElement
          } -> R.ReactElement
topbar
  { toURI
  , openLogin
  , windowSizeSignal
  , siteLinks
  , mobileMenuButtonSignal
  , currentPageSignal
  , authTokenSignal
  , userDetailsSignal
  , imageSrc
  , buttons
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        , initAuthToken: unsafePerformEff $ IxSignal.get authTokenSignal
        , initUserDetails: unsafePerformEff $ IxSignal.get userDetailsSignal
        }
      {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , openLogin
          , siteLinks
          , currentPageSignal
          , windowSizeSignal
          , authTokenSignal
          , userDetailsSignal
          , mobileMenuButtonSignal
          , imageSrc
          , buttons
          }
        )
        (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
        $ Signal.whileMountedIxUUID
            userDetailsSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedUserDetails x))
        $ Signal.whileMountedIxUUID
            authTokenSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedAuthToken x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
