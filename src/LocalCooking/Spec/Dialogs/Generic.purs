module LocalCooking.Spec.Dialogs.Generic where

import LocalCooking.Spec.Form.Pending (pending)
import LocalCooking.Spec.Form.Submit as Submit
import LocalCooking.Spec.Snackbar (SnackbarMessage)
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingAction, LocalCookingState, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Window (WindowSize (..))
import LocalCooking.Links.Class (class LocalCookingSiteLinks, class ToLocation)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.Time.Duration (Milliseconds (..))
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue
import DOM (DOM)

import MaterialUI.Types (createStyles)
import MaterialUI.Dialog (dialog)
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Button (button)
import MaterialUI.Button as Button

import Queue.Types (readOnly, allowReading)
import Queue (READ, WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)



type State input siteLinks userDetails =
  { open         :: Maybe input
  , localCooking :: LocalCookingState siteLinks userDetails
  }


initialState :: forall input siteLinks userDetails
              . LocalCookingState siteLinks userDetails -> State input siteLinks userDetails
initialState localCooking =
  { open: Nothing
  , localCooking
  }


data Action input siteLinks userDetails
  = Open input
  | Close
  | Submit
  | LocalCookingAction (LocalCookingAction siteLinks userDetails)

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , dom       :: DOM
  | eff)


getLCState :: forall input siteLinks userDetails
            . Lens' (State input siteLinks userDetails) (LocalCookingState siteLinks userDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })

getLCAction :: forall input siteLinks userDetails
             . Prism' (Action input siteLinks userDetails) (LocalCookingAction siteLinks userDetails)
getLCAction = prism' LocalCookingAction $ case _ of
  LocalCookingAction x -> Just x
  _ -> Nothing


spec :: forall eff siteLinks userDetails userDetailsLinks input output
      . LocalCookingSiteLinks siteLinks userDetailsLinks
     => ToLocation siteLinks
     => LocalCookingParams siteLinks userDetails (Effects eff)
     -> { dialogOutputQueue :: One.Queue (write :: WRITE) (Effects eff) (Maybe output)
        , closeQueue :: Maybe (One.Queue (write :: WRITE) (Effects eff) Unit)
        , content ::
          { component ::
            { submitDisabled :: Boolean -> Eff (Effects eff) Unit
            , input          :: input
            } -> Array R.ReactElement
          , obtain    :: Aff (Effects eff) (Maybe output)
          , reset     :: Eff (Effects eff) Unit
          }
        , buttons ::
          { close :: Eff (Effects eff) Unit
          } -> Array R.ReactElement
        , title :: String
        , submit ::
          { disabledSignal :: IxSignal (Effects eff) Boolean
          , queue          :: IxQueue (read :: READ) (Effects eff) Unit
          , value          :: String
          }
        , pendingSignal :: Maybe (IxSignal (Effects eff) Boolean)
        }
     -> T.Spec (Effects eff) (State input siteLinks userDetails) Unit (Action input siteLinks userDetails)
spec
  {toURI}
  { submit
  , content
  , pendingSignal
  , dialogOutputQueue
  , closeQueue
  , buttons
  , title
  } = T.simpleSpec (performAction <> performActionLocalCooking getLCState getLCAction) render
  where
    performAction action props state = case action of
      Open x -> void $ T.cotransform _ { open = Just x }
      Close -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p  -> liftEff (IxSignal.set false p)
        void $ T.cotransform _ { open = Nothing }
        liftBase $ delay $ Milliseconds 2000.0
        liftEff content.reset
      Submit -> do
        case pendingSignal of
          Nothing -> pure unit
          Just p -> liftEff (IxSignal.set true p)
        mOutput <- liftBase content.obtain
        case mOutput of
          Nothing -> pure unit -- FIXME error out?
          Just output -> do
            case closeQueue of
              Nothing -> performAction Close props state
              Just closeQueue' -> pure unit
            liftEff (One.putQueue dialogOutputQueue (Just output))
      _ -> pure unit

    render :: T.Render (State input siteLinks userDetails) Unit (Action input siteLinks userDetails)
    render dispatch props state children =
      [ let dialog' =
              if state.localCooking.windowSize < Laptop
              then
                dialog
                  { open: case state.open of
                      Nothing -> false
                      Just _ -> true
                  , fullScreen: true
                  }
              else
                dialog
                  { open: case state.open of
                      Nothing -> false
                      Just _ -> true
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> do
                      pending <- do
                        case pendingSignal of
                          Nothing -> pure false
                          Just p  -> unsafeCoerceEff (IxSignal.get p)
                      when (not pending) $ do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text title]
            , dialogContent {style: createStyles {position: "relative"}} $
                ( case state.open of
                    Nothing -> [R.text ""]
                    Just input ->
                      content.component
                      { submitDisabled: \d -> IxSignal.set d submit.disabledSignal
                      , input
                      }
                ) <>
                  case pendingSignal of
                    Nothing -> []
                    Just p  ->
                      [ pending
                        { pendingSignal: p
                        }
                      ]
            , dialogActions {} $
                buttons
                { close: do
                    unsafeCoerceEff (dispatch Close)
                    One.putQueue dialogOutputQueue Nothing
                } <>
                  [ Submit.submit
                    { color: Button.primary
                    , variant: Button.flat
                    , size: Button.medium
                    , style: createStyles {}
                    , triggerQueue: submit.queue
                    , disabledSignal: submit.disabledSignal
                    } [R.text submit.value]
                  , button
                    { color: Button.default
                    , onTouchTap: mkEffFn1 \_ -> do
                        unsafeCoerceEff (One.putQueue dialogOutputQueue Nothing)
                        dispatch Close
                    } [R.text "Cancel"]
                  ]
            ]
      ]



genericDialog :: forall eff siteLinks userDetails userDetailsLinks input output
               . LocalCookingSiteLinks siteLinks userDetailsLinks
              => ToLocation siteLinks
              => LocalCookingParams siteLinks userDetails (Effects eff)
              -> { dialogQueue       :: OneIO.IOQueues (Effects eff) input (Maybe output)
                 , closeQueue        :: Maybe (One.Queue (write :: WRITE) (Effects eff) Unit)
                 , buttons           ::
                    { close :: Eff (Effects eff) Unit
                    } -> Array R.ReactElement
                 , title             :: String
                 , submitValue       :: String
                 , pends             :: Boolean
                 , content ::
                   { component ::
                     { submitDisabled :: Boolean -> Eff (Effects eff) Unit
                     , input          :: input
                     } -> Array R.ReactElement
                   , obtain    :: Aff (Effects eff) (Maybe output)
                   , reset     :: Eff (Effects eff) Unit
                   }
                 }
              -> R.ReactElement
genericDialog
  params
  { dialogQueue: OneIO.IOQueues {input: dialogInputQueue, output: dialogOutputQueue}
  , closeQueue
  , content
  , submitValue
  , buttons
  , title
  , pends
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { closeQueue
            , buttons
            , title
            , submit:
              { queue: submitQueue
              , disabledSignal: submitDisabledSignal
              , value: submitValue
              }
            , content
            , pendingSignal
            , dialogOutputQueue
            } )
          (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            getLCAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        $ Queue.whileMountedOne
            dialogInputQueue
            (\this x -> unsafeCoerceEff $ dispatcher this $ Open x)
        $ ( case closeQueue of
              Nothing -> id
              Just closeQueue' ->
                Queue.whileMountedOne
                  (allowReading closeQueue')
                  (\this _ -> unsafeCoerceEff $ dispatcher this Close)
          )
        $ Queue.whileMountedIxUUID
            submitQueue
            (\this _ -> unsafeCoerceEff $ dispatcher this Submit)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
  where
    submitDisabledSignal = unsafePerformEff $ IxSignal.make false
    pendingSignal = if pends then unsafePerformEff (Just <$> IxSignal.make false) else Nothing
    submitQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
