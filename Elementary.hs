{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Actions describing elementary row/column operations
module Elementary where

import Data.Kind
import Data.Nat
import Data.Fin (Fin(..))
import qualified Data.Fin as F

type EOperable :: Type -> Nat -> Nat -> Type -> Constraint
class EOperable m i j a | m -> i j a where
  eroSwap :: m -> Fin i -> Fin i -> m -- swaps indices
  eroScale :: m -> a -> Fin i -> m -- scales on index
  eroAdd:: m -> a -> Fin i -> Fin i -> m -- adds to the second index

  ecoSwap :: m -> Fin j -> Fin j -> m -- swaps indices
  ecoScale :: m -> a -> Fin j -> m -- scales on index
  ecoAdd:: m -> a -> Fin j -> Fin j -> m -- adds to the second index

