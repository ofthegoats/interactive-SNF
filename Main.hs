-- | main module, using which the program should be run

module Main where

import Parser
import Interpreter
import SNF
import Matrix

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three

main :: IO ()
main = undefined
