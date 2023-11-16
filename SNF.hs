{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Triple structure to carry out unimodular EROs on all three positions
module SNF where

import Data.Kind
import Data.Nat
import Data.Fin (Fin (..))
import qualified Data.Fin as F

import Matrix
import Elementary
import Latex

type SNF :: Nat -> Nat -> Type -> Type
data SNF m n a = SNF
  { eros :: Matrix m m a
  , ecos :: Matrix n n a
  , base :: Matrix m n a
  }

mkSNF :: Num a => Matrix m n a -> SNF m n a
mkSNF m = SNF
  { eros = M $ \i j -> if i == j then 1 else 0
  , ecos = M $ \i j -> if i == j then 1 else 0
  , base = m
  }

instance (Show a, Show (Matrix (S m) (S n) a), Show (Matrix (S m) (S m) a), Show (Matrix (S n) (S n) a))
  => Show (SNF (S m) (S n) a) where
  show :: SNF (S m) (S n) a -> String
  show SNF{..} = show eros <> "\n --- \n" <> show base <> "\n --- \n" <> show ecos

instance (Num a, EOperable (Matrix m n a) m n a)
  => EOperable (SNF m n a) m n a where
  eroSwap s@SNF{..} a a' = s { eros = eroSwap eros a a' , base = eroSwap base a a' }
  eroScale s@SNF{..} c a = s { eros = eroScale eros c a , base = eroScale base c a }
  eroAdd s@SNF{..} c a a' = s { eros = eroAdd eros c a a' , base = eroAdd base c a a' }

  ecoSwap s@SNF{..} b b' = s { ecos = ecoSwap ecos b b' , base = ecoSwap base b b' }
  ecoScale s@SNF{..} c b = s { ecos = ecoScale ecos c b , base = ecoScale base c b }
  ecoAdd s@SNF{..} c b b' = s { ecos = ecoAdd ecos c b b' , base = ecoAdd base c b b' }

instance (Show a, Latex (Matrix (S m) (S n) a), Latex (Matrix (S m) (S m) a), Latex (Matrix (S n) (S n) a))
  => Latex (SNF (S m) (S n) a) where
  texify :: SNF (S m) (S n) a -> String
  texify SNF{..} =
    "\\left(\n" ++
    texify eros ++ "\n\\left|\n" ++ texify base ++ "\n\\right|\n" ++ texify ecos
    ++ "\n\\right)"
