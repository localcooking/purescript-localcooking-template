module LocalCooking.Spec.Dialogs.PrivacyPolicy where

import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Window (WindowSize (..))
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
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import DOM (DOM)
import DOM.HTML.Types (HISTORY)

import MaterialUI.Types (createStyles)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import Crypto.Scrypt (SCRYPT)

import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal


{-
type State siteLinks =
  { open        :: Boolean
  , windowSize  :: WindowSize
  , currentPage :: siteLinks
  }


initialState :: forall siteLinks
              . {initSiteLinks :: siteLinks, initWindowSize :: WindowSize} -> State siteLinks
initialState {initWindowSize, initSiteLinks} =
  { open: false
  , windowSize: initWindowSize
  , currentPage: initSiteLinks
  }


data Action siteLinks
  = Open
  | Close
  | ChangedWindowSize WindowSize
  | ChangedPage siteLinks
  | SubmitPrivacyPolicy


spec :: forall eff siteLinks userDetailsLinks
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => { privacyPolicySignal :: IxSignal (Effects eff) Boolean
        , toURI             :: Location -> URI
        }
     -> T.Spec (Effects eff) (State siteLinks) Unit (Action siteLinks)
spec
  { privacyPolicySignal
  , toURI
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close ->
        void $ T.cotransform _ { open = false }
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      SubmitPrivacyPolicy -> do
        liftEff $ IxSignal.set false privacyPolicySignal
        performAction Close props state

    render :: T.Render (State siteLinks) Unit (Action siteLinks)
    render dispatch props state children =
      [ let dialog' =
              if state.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Privacy Policy"]
            , dialogContent {style: createStyles {position: "relative"}}
              [ R.iframe
                [ RP.src $ URI.print $ toURI $ toLocation PrivacyPolicyLink
                , RP.style {width: "100%", border: "1px solid black"}
                ] []
              ]
            , dialogActions {}
              [ button
                { color: Button.primary
                , onTouchTap: mkEffFn1 \_ -> dispatch SubmitPrivacyPolicy
                } [R.text "Acknowledge"]
              ]
            ]
      ]
-}


type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  , dom       :: DOM
  , history   :: HISTORY
  | eff)



privacyPolicyDialog :: forall eff siteLinks userDetailsLinks
             . LocalCookingSiteLinks siteLinks userDetailsLinks
            => ToLocation siteLinks
            => { privacyPolicySignal :: IxSignal (Effects eff) Boolean
               , errorMessageQueue   :: One.Queue (write :: WRITE) (Effects eff) SnackbarMessage
               , windowSizeSignal    :: IxSignal (Effects eff) WindowSize
               , currentPageSignal   :: IxSignal (Effects eff) siteLinks
               , toURI               :: Location -> URI
               , env                 :: Env
               }
            -> R.ReactElement
privacyPolicyDialog
  { privacyPolicySignal
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
  , toURI
  , env
  } =
  genericDialog
  { dialogQueue: privacyPolicyQueue
  , errorMessageQueue
  , windowSizeSignal
  , currentPageSignal
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
{-
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { toURI
            , privacyPolicySignal
            } )
          (initialState init)
      reactSpecPrivacyPolicy =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Signal.whileMountedIxUUID
            privacyPolicySignal
            (\this p -> unsafeCoerceEff $ dispatcher this (if p then Open else Close))
            reactSpec
  in  R.createElement (R.createClass reactSpecPrivacyPolicy) unit []
-}
  where
    privacyPolicyQueue = unsafePerformEff $ do
      x@(OneIO.IOQueues {input}) <- OneIO.newIOQueues
      IxSignal.subscribe
        (\n -> when n (One.putQueue (One.allowWriting input) unit))
        privacyPolicySignal
      pure x
