module LocalCooking.Spec.Dialogs.PrivacyPolicy where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Window (WindowSize)
import LocalCooking.Links (PolicyLinks (..))
import LocalCooking.Links.Class (toLocation, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Spec.Snackbar (SnackbarMessage)
import LocalCooking.Types.Env (Env)

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import DOM (DOM)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)



privacyPolicyDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { privacyPolicyDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
               , errorMessageQueue        :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
               , windowSizeSignal         :: IxSignal (Effects eff) WindowSize
               , currentPageSignal        :: IxSignal (Effects eff) siteLinks
               , toURI                    :: Location -> URI
               , env                      :: Env
               }
            -> R.ReactElement
privacyPolicyDialog
  { privacyPolicyDialogQueue
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  } =
  genericDialog
  { dialogQueue: privacyPolicyDialogQueue
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , closeQueue: Nothing
  , toURI
  , env
  , buttons: \_ -> []
  , title: "Privacy Policy"
  , submitValue: "Acknowledge"
  , pends: false
  , content:
    { component: \_ ->
      [ R.iframe
        [ RP.src $ URI.print $ toURI $ toLocation PrivacyPolicyLink
        , RP.style {width: "100%", border: "1px solid black"}
        ] []
      ]
    , obtain: pure (Just unit)
    , reset: pure unit
    }
  }
