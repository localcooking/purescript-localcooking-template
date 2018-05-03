module Control.Monad.Eff.Ref.Extra where

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef)


takeRef :: forall eff a. Ref (Maybe a) -> Eff (ref :: REF | eff) (Maybe a)
takeRef ref = do
  mX <- readRef ref
  writeRef ref Nothing
  pure mX
