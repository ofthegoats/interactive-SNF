{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Interpreter where

import Data.Kind
import Data.Nat
import Data.Fin (Fin(..))
import qualified Data.Fin as F

import Matrix
import Elementary

type Index :: Nat -> Nat -> Type
data Index m n = Row (Fin m) | Column (Fin n) deriving Show

type Operation :: Nat -> Nat -> Type -> Type
data Operation m n a where
  EROSwap :: Fin m -> Fin m -> Operation m n a
  EROScale :: a -> Fin m -> Operation m n a
  EROAdd :: a -> Fin m -> Fin m -> Operation m n a
  ECOSwap :: Fin n -> Fin n -> Operation m n a
  ECOScale :: a -> Fin n -> Operation m n a
  ECOAdd :: a -> Fin n -> Fin n -> Operation m n a
  deriving Show

runOperation :: EOperable h m n a => Operation m n a -> h -> h
runOperation (EROSwap i j) m = eroSwap m i j
runOperation (EROScale c i) m = eroScale m c i
runOperation (EROAdd c i j) m = eroAdd m c i j
runOperation (ECOSwap i j) m = ecoSwap m i j
runOperation (ECOScale c i) m = ecoScale m c i
runOperation (ECOAdd c i j) m = ecoAdd m c i j
