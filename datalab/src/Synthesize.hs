{-# LANGUAGE OverloadedStrings #-}

-- | Module      : Synthesize
--   Description : A synthesizer for bit-level programs using CEGIS.
--   Copyright   : (c) Sirui Lu, 2023
--   License     : BSD3
--   Maintainer  : siruilu@cs.washington.edu
--   Stability   : experimental
--
-- This module uses Grisette's built-in CEGIS engine to synthesize bit-level
-- programs.
module Synthesize (synthesize) where

import Control.Monad (replicateM)
import Control.Monad.Except (withExceptT)
import Grisette
  ( EvaluateSym (evaluateSym),
    GenSymSimple (simpleFresh),
    GrisetteSMTConfig,
    SEq ((==~)),
    VerificationConditions (AssertionViolation),
    cegisExceptStdVC,
    runFresh,
    symAssert,
  )
import Program
  ( Prog (..),
    SymVal,
    interpretProg,
  )

-- | Synthesize a program that satisfies the given specification.
synthesize ::
  -- | The configuration for the SMT solver.
  GrisetteSMTConfig n ->
  -- | The specification.
  ([SymVal] -> SymVal) ->
  -- | The program sketch to be synthesized.
  Prog ->
  IO (Maybe Prog)
synthesize config spec prog = do
  res <-
    cegisExceptStdVC
      config
      -- We will synthesize a program that works on **all** inputs. Here is how
      -- you specify the @forall@.
      inputs
      ( do
          evalResult <-
            -- We use `withExceptT` to convert the error type from `Error` to
            -- `VerificationCondition`. The `cegisExceptStdVC` function will
            -- synthesize a program that does not cause an assertion violation.
            withExceptT (const AssertionViolation) $
              interpretProg inputs prog
          symAssert (evalResult ==~ spec inputs)
      )
  case res of
    (_, Left _) -> return Nothing
    (_, Right m) ->
      -- If the synthesis succeeds and give us a model, we evaluate the program
      -- sketch on the model to get the synthesized program.
      return $ Just $ evaluateSym True m prog
  where
    -- We generate fresh symbolic values for the input variables.
    inputs :: [SymVal]
    inputs =
      flip runFresh "inputs" $
        replicateM (progInNum prog) (simpleFresh ())
