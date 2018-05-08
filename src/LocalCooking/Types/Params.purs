module LocalCooking.Types.Params where

import LocalCooking.Window (WindowSize)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.URI.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', Prism', review, clonePrism, matching, (%~))
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import React (ReactSpec, ReactThis)
import React.Signal.WhileMounted as Signal
import Thermite as T



type LocalCookingParams siteLinks userDetails eff =
  { toURI             :: Location -> URI
  , siteLinks         :: siteLinks -> Eff eff Unit
  , currentPageSignal :: IxSignal eff siteLinks
  , windowSizeSignal  :: IxSignal eff WindowSize
  , authTokenSignal   :: IxSignal eff (Maybe AuthToken)
  , userDetailsSignal :: IxSignal eff (Maybe userDetails)
  }


type LocalCookingState siteLinks userDetails =
  { currentPage :: siteLinks
  , windowSize  :: WindowSize
  , authToken   :: Maybe AuthToken
  , userDetails :: Maybe userDetails
  }


initLocalCookingState :: forall siteLinks userDetails eff
                       . LocalCookingParams siteLinks userDetails (ref :: REF | eff)
                      -> Eff (ref :: REF | eff) (LocalCookingState siteLinks userDetails)
initLocalCookingState {currentPageSignal,windowSizeSignal,authTokenSignal,userDetailsSignal} = do
  currentPage <- IxSignal.get currentPageSignal
  windowSize <- IxSignal.get windowSizeSignal
  authToken <- IxSignal.get authTokenSignal
  userDetails <- IxSignal.get userDetailsSignal
  pure
    { currentPage
    , windowSize
    , authToken
    , userDetails
    }


data LocalCookingAction siteLinks userDetails
  = ChangedCurrentPage siteLinks
  | ChangedWindowSize WindowSize
  | ChangedAuthToken (Maybe AuthToken)
  | ChangedUserDetails (Maybe userDetails)


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


performActionLocalCooking :: forall siteLinks userDetails eff state action props
                           . Lens' state (LocalCookingState siteLinks userDetails)
                          -> Prism' action (LocalCookingAction siteLinks userDetails)
                          -> T.PerformAction eff state props action
performActionLocalCooking getLCState getLCAction action props state =
  let go :: LocalCookingState siteLinks userDetails -> LocalCookingState siteLinks userDetails
      go = case matching (clonePrism getLCAction) action of
        Left _ -> id
        Right a -> case a of
          ChangedCurrentPage x -> _ { currentPage = x }
          ChangedWindowSize x -> _ { windowSize = x }
          ChangedAuthToken x -> _ { authToken = x }
          ChangedUserDetails x -> _ { userDetails = x }
  in  void (T.cotransform (getLCState %~ go))


whileMountedLocalCooking :: forall siteLinks userDetails eff state action props render
                          . LocalCookingParams siteLinks userDetails (Effects eff)
                         -> Prism' action (LocalCookingAction siteLinks userDetails)
                         -> (ReactThis props state -> action -> Eff (Effects eff) Unit)
                         -> ReactSpec props state render (Effects eff)
                         -> ReactSpec props state render (Effects eff)
whileMountedLocalCooking
  { currentPageSignal
  , windowSizeSignal
  , authTokenSignal
  , userDetailsSignal
  }
  getLCAction
  dispatcher
  reactSpec
  = Signal.whileMountedIxUUID
      currentPageSignal
      (\this x -> dispatcher this (review (clonePrism getLCAction) (ChangedCurrentPage x)))
  $ Signal.whileMountedIxUUID
      windowSizeSignal
      (\this x -> dispatcher this (review (clonePrism getLCAction) (ChangedWindowSize x)))
  $ Signal.whileMountedIxUUID
      authTokenSignal
      (\this x -> dispatcher this (review (clonePrism getLCAction) (ChangedAuthToken x)))
  $ Signal.whileMountedIxUUID
      userDetailsSignal
      (\this x -> dispatcher this (review (clonePrism getLCAction) (ChangedUserDetails x)))
      reactSpec
