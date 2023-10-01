{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module: DPSynth.Experimental.CEGIS
--
-- This module provides an alternative implementation of the CEGIS algorithm.
-- We may integrate this into the Grisette system in the future after we figured
-- out a good interface.
--
-- This modules uses internal interfaces of Grisette.
module DPSynth.Experimental.CEGIS
  ( CEGISResult (..),
    VerificationResult (..),
    CEGISAlgorithm (..),
    cegis,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import qualified Data.Text as T
import Grisette
  ( EvaluateSym,
    FreshIdent (FreshIdentWithInfo),
    GrisetteSMTConfig (sbvConfig),
    Solvable (con),
    SymBool (SymBool),
    ToCon,
    evaluateSymToCon,
  )
import Grisette.Backend.SBV.Data.SMT.Lowering
  ( lowerSinglePrim,
    lowerSinglePrimCached,
    parseModel,
  )
import Grisette.Backend.SBV.Data.SMT.SymBiMap (SymBiMap)

data CEGISResult cex conProg
  = CEGISError T.Text
  | CEGISFailedWithCex cex
  | CEGISSuccess conProg
  deriving (Show)

data VerificationResult cex
  = VerificationError T.Text
  | VerificationFoundCex cex
  | VerificationSuccess

class
  CEGISAlgorithm problem cex synthConditionState verifierState symProg conProg
    | problem -> cex synthConditionState verifierState symProg conProg
  where
  synthCondition ::
    problem ->
    synthConditionState ->
    symProg ->
    cex ->
    FreshIdent ->
    (SymBool, synthConditionState)
  runVerifier ::
    problem ->
    verifierState ->
    conProg ->
    IO (VerificationResult cex, verifierState)
  initialSynthConditionState :: problem -> synthConditionState
  initialVerifierState :: problem -> verifierState

cegis ::
  forall n problem cex synthConditionState verifierState symProg conProg.
  ( CEGISAlgorithm
      problem
      cex
      synthConditionState
      verifierState
      symProg
      conProg,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  GrisetteSMTConfig n ->
  problem ->
  symProg ->
  IO (CEGISResult cex conProg)
cegis config problem symProg =
  SBV.runSMTWith (sbvConfig config) $ do
    let SymBool t = con True
    (symbolMap, a) <- lowerSinglePrim config t
    SBVC.query $ do
      SBV.constrain a
      r <- SBVC.checkSat
      case r of
        SBVC.Sat -> do
          md <- SBVC.getModel
          let model = parseModel config md symbolMap
          loop
            0
            (initialSynthConditionState problem)
            (initialVerifierState problem)
            (evaluateSymToCon model symProg)
            symbolMap
        SBVC.Unsat ->
          error "Should not happen. Check if your solver is working correctly."
        SBVC.Unk -> return $ CEGISError "Solver returned unknown status."
        _ ->
          return $
            CEGISError $
              "Solver returned unexpected status: " <> T.pack (show r)
  where
    loop ::
      Int ->
      synthConditionState ->
      verifierState ->
      conProg ->
      SymBiMap ->
      SBVC.Query (CEGISResult cex conProg)
    loop n synthConditionState verifierState conProg symbolMap = do
      r <- liftIO $ runVerifier problem verifierState conProg
      case r of
        (VerificationSuccess, _) -> return $ CEGISSuccess conProg
        (VerificationError err, _) ->
          return $ CEGISError $ "Verifier error: " <> err
        (VerificationFoundCex cex, verifierState') -> do
          let (SymBool t, synthConditionState') =
                synthCondition
                  problem
                  synthConditionState
                  symProg
                  cex
                  (FreshIdentWithInfo "cegis" n) -- TODO: use unique identifier
          (symbolMap', lowered) <- lowerSinglePrimCached config t symbolMap
          SBV.constrain lowered
          checkSatResult <- SBVC.checkSat
          case checkSatResult of
            SBVC.Sat -> do
              md <- SBVC.getModel
              let model = parseModel config md symbolMap'
              loop
                (n + 1)
                synthConditionState'
                verifierState'
                (evaluateSymToCon model symProg)
                symbolMap'
            SBVC.Unsat -> return $ CEGISFailedWithCex cex
            SBVC.Unk -> return $ CEGISError "Solver returned unknown status."
            _ ->
              return $
                CEGISError $
                  "Solver returned unexpected status."
                    <> T.pack (show checkSatResult)
