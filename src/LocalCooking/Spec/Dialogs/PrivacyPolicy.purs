module LocalCooking.Spec.Dialogs.PrivacyPolicy where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Links (PolicyLinks (..))
import LocalCooking.Links.Class (toLocation, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Spec.Snackbar (SnackbarMessage)
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams)

import Prelude
import Data.URI.URI (print) as URI
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import DOM (DOM)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO



type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)



privacyPolicyDialog :: forall eff siteLinks userDetails userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => LocalCookingParams siteLinks userDetails (Effects eff)
            -> { privacyPolicyDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe Unit)
               , errorMessageQueue        :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
               , env                      :: Env
               }
            -> R.ReactElement
privacyPolicyDialog
  params@{toURI}
  { privacyPolicyDialogQueue
  , errorMessageQueue
  , env
  } =
  genericDialog
  params
  { dialogQueue: privacyPolicyDialogQueue
  , errorMessageQueue
  , closeQueue: Nothing
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
