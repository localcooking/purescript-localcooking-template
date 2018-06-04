module LocalCooking.Types.Params where

import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.URI.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', Prism', review, clonePrism, matching, (%~))
import Data.UUID (GENUUID)
import Data.Generic (class Generic, gShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import React (ReactSpec, ReactThis)
import React.Signal.WhileMounted as Signal
import DOM.HTML.Window.Extra (WindowSize)
import Thermite as T



-- | Universal arguments for each React component process
type LocalCookingParams siteLinks userDetails eff =
  { toURI             :: Location -> URI
  , siteLinks         :: siteLinks -> Eff eff Unit
  , currentPageSignal :: IxSignal eff siteLinks
  , windowSizeSignal  :: IxSignal eff WindowSize
  , authTokenSignal   :: IxSignal eff (Maybe AuthToken)
  , userDetailsSignal :: IxSignal eff (Maybe userDetails)
  }


-- | Storable state for each Thermite action schema
type LocalCookingState siteLinks userDetails =
  { currentPage :: siteLinks
  , windowSize  :: WindowSize
  , authToken   :: Maybe AuthToken
  , userDetails :: Maybe userDetails
  }


-- | Obtain state from params
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


-- | Modifications to state
data LocalCookingAction siteLinks userDetails
  = ChangedCurrentPage siteLinks
  | ChangedWindowSize WindowSize
  | ChangedAuthToken (Maybe AuthToken)
  | ChangedUserDetails (Maybe userDetails)

derive instance genericLocalCookingAction :: (Generic siteLinks, Generic userDetails) => Generic (LocalCookingAction siteLinks userDetails)

instance showLocalCookingAction :: (Generic siteLinks, Generic userDetails) => Show (LocalCookingAction siteLinks userDetails) where
  show = gShow


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


-- | Enact modifications to state
performActionLocalCooking :: forall siteLinks userDetails eff state action props
                           . Lens' state (LocalCookingState siteLinks userDetails)
                          -> T.PerformAction eff state props (LocalCookingAction siteLinks userDetails)
performActionLocalCooking getLCState a props state =
  let go :: LocalCookingState siteLinks userDetails -> LocalCookingState siteLinks userDetails
      go = case a of
        ChangedCurrentPage x -> _ { currentPage = x }
        ChangedWindowSize x -> _ { windowSize = x }
        ChangedAuthToken x -> _ { authToken = x }
        ChangedUserDetails x -> _ { userDetails = x }
  in  void (T.cotransform (getLCState %~ go))


-- | Bind Thermite action modifications to the React component signals
whileMountedLocalCooking :: forall siteLinks userDetails eff state action props render
                          . LocalCookingParams siteLinks userDetails (Effects eff)
                         -> String
                         -> (LocalCookingAction siteLinks userDetails -> action)
                         -> (ReactThis props state -> action -> Eff (Effects eff) Unit)
                         -> ReactSpec props state render (Effects eff)
                         -> ReactSpec props state render (Effects eff)
whileMountedLocalCooking
  { currentPageSignal
  , windowSizeSignal
  , authTokenSignal
  , userDetailsSignal
  }
  k
  buildLCAction
  dispatcher
  reactSpec
  = Signal.whileMountedIx
      currentPageSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedCurrentPage x)))
  $ Signal.whileMountedIx
      windowSizeSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedWindowSize x)))
  $ Signal.whileMountedIx
      authTokenSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedAuthToken x)))
  $ Signal.whileMountedIx
      userDetailsSignal
      k
      (\this x -> dispatcher this (buildLCAction (ChangedUserDetails x)))
      reactSpec
