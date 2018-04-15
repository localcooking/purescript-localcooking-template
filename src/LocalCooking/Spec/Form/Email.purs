module LocalCooking.Spec.Form.Email where

import Prelude
import Data.Maybe (Maybe (..))
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)

import Thermite as T
import React as R
import React.DOM as R

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.TextField (textField)

import Unsafe.Coerce (unsafeCoerce)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State =
  { email :: String
  }

initialState :: State
initialState =
  { email: ""
  }

data Action
  = ChangedEmail String
  | EmailUnfocused

type Effects eff =
  ( ref :: REF
  | eff)

spec :: forall eff
      . { emailSignal :: IxSignal (Effects eff) (Maybe (Maybe EmailAddress))
        , parentSignal :: Maybe (IxSignal (Effects eff) (Maybe (Maybe EmailAddress)))
        , label :: R.ReactElement
        , fullWidth :: Boolean
        , name :: String
        , id :: String
        } -> T.Spec (Effects eff) State Unit Action
spec
  { emailSignal
  , parentSignal
  , label
  , fullWidth
  , name
  , id
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedEmail e -> do
        liftEff $ IxSignal.set Nothing emailSignal
        void $ T.cotransform _ { email = e }
      EmailUnfocused -> liftEff $ case emailAddress state.email of
        Nothing -> IxSignal.set (Just Nothing) emailSignal
        Just e -> IxSignal.set (Just (Just e)) emailSignal

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ textField
        { label
        , fullWidth
        , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
        , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
        , error: case unsafePerformEff (IxSignal.get emailSignal) of
          Nothing -> false
          Just mEmail -> case mEmail of
            Nothing -> true
            Just e -> case parentSignal of
              Nothing -> false
              Just parentSignal' -> case unsafePerformEff (IxSignal.get parentSignal') of
                Nothing -> true
                Just mEmailParent -> case mEmailParent of
                  Nothing -> true
                  Just e2 -> e /= e2
        , name
        , id
        }
      ]



email :: forall eff
       . { label :: R.ReactElement
         , fullWidth :: Boolean
         , name :: String
         , id :: String
         , emailSignal :: IxSignal (Effects eff) (Maybe (Maybe EmailAddress))
         , parentSignal :: Maybe (IxSignal (Effects eff) (Maybe (Maybe EmailAddress))) --for confirm
         } -> R.ReactElement
email params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
