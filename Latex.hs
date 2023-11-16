{-# LANGUAGE StandaloneKindSignatures #-}

module Latex where

import Data.Kind

-- | "enhanced Show instance" where the string is in LaTeX, which can then be copied over and compiled
type Latex :: Type -> Constraint
class Latex a where
  texify :: a -> String
