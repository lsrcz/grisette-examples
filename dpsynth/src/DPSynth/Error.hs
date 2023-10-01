{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- | Module: DPSynth.Error
--
-- This module provides the definition of dynamic programming interpretation
-- errors.
module DPSynth.Error (DPSynthError (..)) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), SEq, ToCon, ToSym)
import Grisette.Core.Data.Class.Mergeable (Mergeable)

data DPSynthError
  = UndefinedVar
  | BadStmtOutputs
  | BadProgInputs
  | BadNumOfOperands
  | UnknownOperator T.Text
  | BadSemantics
  | NotOptimal
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, ToSym DPSynthError, ToCon DPSynthError, SEq)
    via Default DPSynthError
