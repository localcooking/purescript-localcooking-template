module LocalCooking.Spec.Drawers.LeftMenu where

import LocalCooking.Window (WindowSize)
import LocalCooking.Links.Class (class LocalCookingSiteLinks, rootLink)

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.Time.Duration (Milliseconds (..))
import Data.DateTime.Instant (unInstant)
import Control.Monad.Eff (Eff)
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
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal


type State =
  { open :: Boolean
  , windowSize :: WindowSize
  }

initialState :: {initWindowSize :: WindowSize} -> State
initialState {initWindowSize} =
  { open: false
  , windowSize: initWindowSize
  }

data Action siteLinks
  = ChangedWindowSize WindowSize
  | Clicked siteLinks
  | Open
  | Close


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  , now :: NOW
  | eff)


spec :: forall eff siteLinks
      . LocalCookingSiteLinks siteLinks
     => { siteLinks :: siteLinks -> Eff (Effects eff) Unit
        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
        , toURI :: Location -> URI
        , currentPageSignal :: IxSignal (Effects eff) siteLinks
        , buttons :: { toURI :: Location -> URI
                      , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                      , currentPageSignal :: IxSignal (Effects eff) siteLinks
                      , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                      } -> Array R.ReactElement
        }
     -> T.Spec (Effects eff) State Unit (Action siteLinks)
spec {siteLinks,windowSizeSignal,currentPageSignal,toURI,buttons} = T.simpleSpec performAction render
  where
    lastOpen = unsafePerformEff (newRef Nothing)

    performAction action props state = case action of
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      Clicked x -> do
        performAction Close props state
        liftEff (siteLinks x)
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


    render :: T.Render State Unit (Action siteLinks)
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
          ] <> buttons {toURI,siteLinks,currentPageSignal,windowSizeSignal}
        ]
      ]


leftMenu :: forall eff siteLinks
          . LocalCookingSiteLinks siteLinks
         => { mobileDrawerOpenSignal :: Queue (read :: READ) (Effects eff) Unit
            , siteLinks :: siteLinks -> Eff (Effects eff) Unit
            , windowSizeSignal :: IxSignal (Effects eff) WindowSize
            , toURI :: Location -> URI
            , currentPageSignal :: IxSignal (Effects eff) siteLinks
            , buttons :: { toURI :: Location -> URI
                         , siteLinks :: siteLinks -> Eff (Effects eff) Unit
                         , currentPageSignal :: IxSignal (Effects eff) siteLinks
                         , windowSizeSignal :: IxSignal (Effects eff) WindowSize
                         } -> Array R.ReactElement
            }
         -> R.ReactElement
leftMenu
  { mobileDrawerOpenSignal
  , siteLinks
  , toURI
  , windowSizeSignal
  , currentPageSignal
  , buttons
  } =
  let init =
        { initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            {siteLinks,toURI,windowSizeSignal,currentPageSignal,buttons}
          )
          (initialState init)
      reactSpecLogin =
          Queue.whileMountedOne
            mobileDrawerOpenSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
