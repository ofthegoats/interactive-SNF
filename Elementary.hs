{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Actions describing elementary row/column operations
module Elementary where

import Data.Kind

type EOperable :: Type -> Type -> Type -> Type -> Constraint
class EOperable m i j a | m -> i j a where
  eroSwap :: m -> i -> i -> m -- swaps indices
  eroScale :: m -> a -> i -> m -- scales on index
  eroAdd:: m -> a -> i -> i -> m -- adds to the second index

  ecoSwap :: m -> j -> j -> m -- swaps indices
  ecoScale :: m -> a -> j -> m -- scales on index
  ecoAdd:: m -> a -> j -> j -> m -- adds to the second index

