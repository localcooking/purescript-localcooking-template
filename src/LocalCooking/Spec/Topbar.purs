module LocalCooking.Spec.Topbar where

import LocalCooking.Links.Class (class LocalCookingSiteLinks, class ToLocation, toLocation, rootLink, isUserDetailsLink, userDetailsLink)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Common.AuthToken (AuthToken)

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



type State siteLinks =
  { windowSize :: WindowSize
  , currentPage :: siteLinks
  , userDetails :: Maybe {email :: EmailAddress}
  }

initialState :: forall siteLinks
              . {initWindowSize :: WindowSize, initSiteLinks :: siteLinks} -> State siteLinks
initialState {initWindowSize,initSiteLinks} =
  { windowSize: initWindowSize
  , currentPage: initSiteLinks
  , userDetails: Nothing
  }

data Action siteLinks
  = OpenLogin
  | ClickedMobileMenuButton
  | ChangedWindowSize WindowSize
  | ChangedCurrentPage siteLinks
  | ChangedUserDetails (Maybe {email :: EmailAddress})
  | Clicked siteLinks

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff siteLinks
      . LocalCookingSiteLinks siteLinks
     => ToLocation siteLinks
     => { toURI :: Location -> URI
        , openLoginSignal :: Queue (write :: WRITE) (Effects eff) Unit
        , siteLinks :: siteLinks -> Eff (Effects eff) Unit
        , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
        , currentPageSignal :: IxSignal (Effects eff) siteLinks
        , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
        , imageSrc :: Location
        , buttons :: { toURI :: Location -> URI
                      , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                      , currentPageSignal :: IxSignal (Effects eff) siteLinks
                      , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                      , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                      } -> Array R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { toURI
  , openLoginSignal
  , siteLinks
  , mobileMenuButtonSignal
  , windowSizeSignal
  , currentPageSignal
  , authTokenSignal
  , imageSrc
  , buttons
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff (putQueue openLoginSignal unit)
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonSignal unit)
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedCurrentPage x -> void $ T.cotransform _ { currentPage = x }
      ChangedUserDetails x -> void $ T.cotransform _ { userDetails = x }
      Clicked x -> liftEff (siteLinks x)

    render :: T.Render (State siteLinks) Unit (Action siteLinks)
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
                , variant: case unit of
                  _ | state.currentPage == rootLink -> Button.flat
                    | otherwise -> Button.raised
                } [R.text "About"]
              ] <> buttons {siteLinks,toURI,currentPageSignal,windowSizeSignal,authTokenSignal}
          ) <>
          [ R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}] $ case state.userDetails of
               Nothing ->
                [ button
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch OpenLogin
                  } [R.text "Login"]
                ]
               Just {email} ->
                [ button -- TODO cart iconButton
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch $ Clicked $ userDetailsLink :: siteLinks
                  , disabled: case unit of
                    _ | isUserDetailsLink state.currentPage -> true
                      | otherwise -> false
                  } [R.text $ Email.toString email]
                ]
          ]
        ]
      ]



topbar :: forall eff siteLinks
        . LocalCookingSiteLinks siteLinks
       => ToLocation siteLinks
       => { toURI :: Location -> URI
          , openLoginSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
          , currentPageSignal :: IxSignal (Effects eff) siteLinks
          , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
          , siteLinks :: siteLinks -> Eff (Effects eff) Unit
          , imageSrc :: Location
          , buttons :: { toURI :: Location -> URI
                        , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                        , currentPageSignal :: IxSignal (Effects eff) siteLinks
                        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                        , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
                        } -> Array R.ReactElement
          } -> R.ReactElement
topbar
  { toURI
  , openLoginSignal
  , windowSizeSignal
  , siteLinks
  , mobileMenuButtonSignal
  , currentPageSignal
  , authTokenSignal
  , imageSrc
  , buttons
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , openLoginSignal
          , siteLinks
          , currentPageSignal
          , windowSizeSignal
          , authTokenSignal
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
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
