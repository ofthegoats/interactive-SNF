-- | main module, using which the program should be run

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import Data.Nat
import Data.Fin (Fin(..))
import qualified Data.Fin as Fin

import Control.Monad.State.Lazy

import Parser
import Interpreter
import SNF
import Matrix
import Latex

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three

matrices :: SNF Three Four Integer
matrices = mkSNF . M . curry $ \case
  { (0, 0) -> 4 ; (0, 1) -> 6 ; (0, 2) -> 0 ; (0, 3) -> -4
  ; (1, 0) -> 8 ; (1, 1) -> -6 ; (1, 2) -> 4 ; (1, 3) -> 0
  ; (2, 0) -> 0 ; (2, 1) -> 10 ; (2, 2) -> 12 ; (2, 3) -> -8
  }

repl :: (Num a, Read (Operation m n a), Latex (SNF m n a)) => StateT (SNF m n a) IO String
repl = do
  snf :: SNF m n a <- get
  liftIO . putStrLn . texify $ snf
  liftIO $ putStr "> "
  cmd :: String <- liftIO getLine
  if cmd == "done."
    then return "finished." -- TODO replace /return/ with texify and use Show for printing
    else let op = read cmd in do -- TODO make this safe!! annoying when it crashes
      modify (runOperation op)
      repl

main = runStateT repl matrices
