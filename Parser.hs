{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Parser (lineToOp) where

import Data.Char
import Data.Fin (Fin (..))
import Data.Fin qualified as F
import Data.Kind
import Data.List
import Data.Nat
import Data.Type.Nat (SNatI)

import Interpreter

import Text.Read (readMaybe)

-- * operations are of the form
-- * > "scale r3 4"
-- * > "swap r3 r4"
-- * > "add c2 2 c3"
-- * the user shouldn't worry about zero-indexing

strToIndex :: (SNatI m, SNatI n) => String -> Maybe (Index m n)
strToIndex ((toLower -> 'r'):(readMaybe -> Just (i :: Integer))) = Just . Row $ fromIntegral (i - 1)
strToIndex ((toLower -> 'c'):(readMaybe -> Just (i :: Integer))) = Just . Column $ fromIntegral (i - 1)
strToIndex _ = Nothing

lineToOp :: forall (m :: Nat) (n :: Nat) a. (SNatI m, SNatI n, Read a) => String -> Maybe (Operation m n a)
lineToOp (stripPrefix "scale " -> Just (words -> [strToIndex -> Just (el :: Index m n), c])) =
  Just $ case el of
    Row m -> EROScale (read c) m
    Column n -> ECOScale (read c) n
lineToOp (stripPrefix "swap " ->
          Just (words ->
                 [strToIndex -> Just (el1 :: Index m n)
               , strToIndex -> Just (el2 :: Index m n)])) =
  case (el1, el2) of
    (Row _, Column _) -> Nothing
    (Column _, Row _) -> Nothing
    (Row m, Row m') -> Just $ EROSwap m m'
    (Column n, Column n') -> Just $ ECOSwap n n'
lineToOp (stripPrefix "add " ->
          Just (words ->
                [strToIndex -> Just (el1 :: Index m n)
               , c
               , strToIndex -> Just (el2 :: Index m n)])) =
  case (el1, el2) of
    (Row _, Column _) -> Nothing
    (Column _, Row _) -> Nothing
    (Row m, Row m') -> Just $ EROAdd (read c) m m'
    (Column n, Column n') -> Just $ ECOAdd (read c) n n'
lineToOp _ = Nothing

instance (SNatI m, SNatI n, Read a) => Read (Operation m n a) where
  readsPrec :: Int -> ReadS (Operation m n a)
  readsPrec _ (lineToOp -> Just res) = [(res, "")]
