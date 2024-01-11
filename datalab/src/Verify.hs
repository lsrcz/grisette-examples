{-# LANGUAGE OverloadedStrings #-}

-- | Module      : Verify
--   Description : A verifier for bit-level programs using CEGIS.
--   Copyright   : (c) Sirui Lu, 2023
--   License     : BSD3
--   Maintainer  : siruilu@cs.washington.edu
--   Stability   : experimental
--
-- This module verifies a bit-level program using Grisette's interface to SMT
-- solvers.
module Verify (verify) where

import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT)
import Grisette
  ( GenSymSimple (simpleFresh),
    GrisetteSMTConfig,
    SEq ((./=)),
    SolvingFailure (Unsat),
    UnionM,
    evaluateSym,
    runFresh,
    solve,
  )
import Program (Error, Prog (..), SymVal, interpretProg)

-- | Verify a program against a specification.
verify ::
  GrisetteSMTConfig n -> ([SymVal] -> SymVal) -> Prog -> IO (Maybe [SymVal])
verify config spec prog = do
  -- We try to find an input on which the program does not satisfy the
  -- specification or throws an error.
  res <- solve config (evalResult ./= return (spec inputs))
  case res of
    Left Unsat -> return Nothing
    Left _ -> error "Unexpected solver error"
    Right m -> return $ Just (evaluateSym True m inputs)
  where
    inputs :: [SymVal]
    inputs =
      flip runFresh "inputs" $
        replicateM (progInNum prog) (simpleFresh ())
    evalResult :: ExceptT Error UnionM SymVal
    evalResult = interpretProg inputs prog
