module LocalCooking.Spec.Drawers.LeftMenu where

import LocalCooking.Links.Class (class LocalCookingSiteLinks, rootLink)
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.Lens (Lens', Prism', lens, prism')
import Data.Time.Duration (Milliseconds (..))
import Data.DateTime.Instant (unInstant)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, now)

import Thermite as T
import React as R
import React.Queue.WhileMounted as Queue

import MaterialUI.Drawer (drawer)
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.Icons.PersonPin (personPinIcon)

import Queue.One (READ, Queue)


type State siteLinks userDetails =
  { open :: Boolean
  , localCooking :: LocalCookingState siteLinks userDetails
  }

initialState :: forall siteLinks userDetails. LocalCookingState siteLinks userDetails -> State siteLinks userDetails
initialState localCooking =
  { open: false
  , localCooking
  }

data Action siteLinks userDetails
  = Clicked siteLinks
  | Open
  | Close
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , now       :: NOW
  | eff)

getLCState :: forall siteLinks userDetails. Lens' (State siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })

getLCAction :: forall siteLinks userDetails. Prism' (Action siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' LocalCookingAction $ case _ of
  LocalCookingAction x -> Just x
  _ -> Nothing



spec :: forall eff siteLinks userDetailsLinks userDetails
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
        }
     -> T.Spec (Effects eff) (State siteLinks userDetails) Unit (Action siteLinks userDetails)
spec
  params
  { buttons
  } = T.simpleSpec (performAction <> performActionLocalCooking getLCState getLCAction) render
  where
    lastOpen = unsafePerformEff (newRef Nothing)

    performAction action props state = case action of
      Clicked x -> do
        performAction Close props state
        liftEff (params.siteLinks x)
      Open -> do
        liftEff $ do
          n <- unInstant <$> now
          writeRef lastOpen (Just n)
        void $ T.cotransform _ { open = true }
      Close -> do
        mTuple <- liftEff $ do
          n <- unInstant <$> now
          mM <- readRef lastOpen
          case mM of
            Nothing -> pure Nothing
            Just m -> pure $ Just $ Tuple n m
        case mTuple of
          Nothing -> pure unit
          Just (Tuple n m)
            | n - m > Milliseconds 500.0 -> do
                liftEff (writeRef lastOpen Nothing)
                void $ T.cotransform _ { open = false }
            | otherwise -> pure unit
      _ -> pure unit


    render :: T.Render (State siteLinks userDetails) Unit (Action siteLinks userDetails)
    render dispatch props state children =
      [ drawer
        { open: state.open
        , onClose: mkEffFn1 \_ -> dispatch Close
        }
        [ list {} $
          [ listItem
              { button: true
              , onClick: mkEffFn1 \_ -> dispatch $ Clicked $ rootLink :: siteLinks
              }
              [ listItemIcon {} personPinIcon
              , listItemText
                { primary: "About"
                }
              ]
          ] <> buttons params
        ]
      ]


leftMenu :: forall eff siteLinks userDetailsLinks userDetails
          . LocalCookingSiteLinks siteLinks userDetailsLinks
         => LocalCookingParams siteLinks userDetails (Effects eff)
         -> { mobileDrawerOpenSignal :: Queue (read :: READ) (Effects eff) Unit
            , buttons :: LocalCookingParams siteLinks userDetails (Effects eff) -> Array R.ReactElement
            }
         -> R.ReactElement
leftMenu
  params
  { mobileDrawerOpenSignal
  , buttons
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { buttons
            }
          )
          (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpecLogin =
          whileMountedLocalCooking
            params
            getLCAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $ Queue.whileMountedOne
            mobileDrawerOpenSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
