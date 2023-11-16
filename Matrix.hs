{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Provide a Naperian matrix representation
module Matrix (Matrix(..), at) where

import Data.Kind
import Data.Nat
import Data.Fin (Fin (..))
import qualified Data.Fin as F
import Data.List

import Elementary
import Latex

type Matrix :: Nat -> Nat -> Type -> Type
data Matrix m n a = M (Fin m -> Fin n -> a)

at :: Matrix m n a -> (Fin m, Fin n) -> a
(M m) `at` (i, j) = m i j

instance Show a => Show (Matrix One One a) where
  show :: Matrix One One a -> String
  show (M m) = show $ m FZ FZ

instance (Show a, Show (Matrix One (S n) a))
  => Show (Matrix One (S (S n)) a) where
  show :: Matrix One (S (S n)) a -> String
  show (M m) = show (m FZ FZ) <> " " <> show ((M $ \i j -> m i (FS j)) :: Matrix One (S n) a)

instance (Show a, Show (Matrix One (S n) a), Show (Matrix (S m) (S n) a))
  => Show (Matrix (S (S m)) (S n) a) where
  show :: Matrix (S (S m)) (S n) a -> String
  show (M m) = show r1 <> "\n" <> show rest
    where r1 :: Matrix One (S n) a = M $ \_ j -> m FZ j
          rest :: Matrix (S m) (S n) a = M $ \i j -> m (FS i) j

instance (Show a, Show (Matrix (S m) (S n) a)) => Latex (Matrix (S m) (S n) a) where
  texify :: Matrix (S m) (S n) a -> String
  texify m =
    -- use @show m@, split it up: put "&"s in between elements and
    -- "\\"s on the end of the lines before joining it back up
    let
      elements = map words (lines (show m))
      m' = intercalate " \\\\\n" $ map (intercalate " & ") elements
    in "\\begin{matrix}\n" ++ m' ++ "\n\\end{matrix}"

instance Functor (Matrix m n) where
  fmap :: (a -> b) -> Matrix m n a -> Matrix m n b
  fmap f (M m) = M $ \i j -> f (m i j)

instance Num a => EOperable (Matrix m n a) m n a where
  eroSwap :: Matrix m n a -> Fin m -> Fin m -> Matrix m n a
  eroSwap (M m) a b = M $ \i j -> m (if i == a then b else if i == b then a else i) j
  
  eroScale :: Matrix m n a -> a -> Fin m -> Matrix m n a
  eroScale (M m) c a = M $ \i j -> if i == a then c * m i j else m i j
  
  eroAdd :: Matrix m n a -> a -> Fin m -> Fin m -> Matrix m n a
  eroAdd (M m) c a a' = M $ \i j -> if i == a' then c * m a j + m i j else m i j

  ecoSwap :: Matrix m n a -> Fin n -> Fin n -> Matrix m n a
  ecoSwap (M m) a b = M $ \i j -> m i (if j == a then b else if j == b then a else j)

  ecoScale :: Matrix m n a -> a -> Fin n -> Matrix m n a
  ecoScale (M m) c b = M $ \i j -> if j == b then c * m i j else m i j

  ecoAdd :: Matrix m n a -> a -> Fin n -> Fin n -> Matrix m n a
  ecoAdd (M m) c b b' = M $ \i j -> if j == b' then c * m i b + m i j else m i j
