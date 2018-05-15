module LocalCooking.Spec.Topbar where

import LocalCooking.Links.Class (class LocalCookingSiteLinks, class ToLocation, toLocation, rootLink, getUserDetailsLink, userDetailsLink)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.User (class UserDetails, getEmailAddress)
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Lens (Lens', Prism', lens, prism')
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



type State siteLinks userDetails = LocalCookingState siteLinks userDetails

initialState :: forall siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState = id

data Action siteLinks userDetails
  = OpenLogin
  | ClickedMobileMenuButton
  | Clicked siteLinks
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
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
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { openLogin :: Eff (Effects eff) Unit
        , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
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
  params@{siteLinks,toURI}
  { openLogin
  , mobileMenuButtonSignal
  , imageSrc
  , buttons
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff openLogin
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonSignal unit)
      Clicked x -> liftEff (siteLinks x)
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

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
              ] <> buttons params
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
       => LocalCookingParams siteLinks userDetails (Effects eff)
       -> { openLogin :: Eff (Effects eff) Unit
          , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
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
  params
  { openLogin
  , mobileMenuButtonSignal
  , imageSrc
  , buttons
  } =
  let {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          params
          { openLogin
          , mobileMenuButtonSignal
          , imageSrc
          , buttons
          }
        )
        (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
