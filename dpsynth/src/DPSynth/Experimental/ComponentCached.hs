{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DPSynth.Experimental.ComponentCached where

import Control.Monad.Except (runExceptT)
import Control.Monad.State.Lazy (MonadTrans (lift), StateT (runStateT))
import DPSynth.Component
  ( AngelicContext,
    DPProg (dpFinal, dpSubInit, dpSubRec),
    Prog,
    Val,
    runProg,
  )
import qualified DPSynth.Concrete as Concrete
import DPSynth.Experimental.CEGIS
  ( CEGISAlgorithm
      ( initialSynthConditionState,
        initialVerifierState,
        runVerifier,
        synthCondition
      ),
    VerificationResult (VerificationFoundCex, VerificationSuccess),
  )
import DPSynth.Experimental.ConcreteCached
  ( InputGen (InputGen),
    quickCheckDPProgMaximizeCaching,
  )
import DPSynth.Experimental.TreeCache
  ( TreeCache,
    emptyTreeCache,
    insertTreeCache,
    queryTreeCache,
  )
import Data.Either (isRight)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    FreshIdent,
    LogicalOp ((&&~)),
    Mergeable,
    SEq ((==~)),
    SymBool,
    ToSym (toSym),
    UnionPrjOp (toGuardedList),
    mrgReturn,
    runFreshT,
  )
import Grisette.Lib.Control.Monad.State.Class (mrgModify)
import Test.QuickCheck (Gen)

data DPProgStepResult = DPProgStepResult
  { stepSub :: [Val],
    stepOutput :: [Val]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default DPProgStepResult)

runDPProgFromSub ::
  [Val] ->
  [Concrete.Val] ->
  Prog ->
  Prog ->
  [Val] ->
  StateT [DPProgStepResult] AngelicContext [Val]
runDPProgFromSub subVals inputs subIter final =
  go inputs subVals
  where
    go [] _ r = mrgReturn r
    go (x : xs) currSubVals r = do
      newSubVals <- lift $ runProg subIter (toSym x : currSubVals)
      newOutput <- lift $ runProg final newSubVals
      mrgModify (++ [DPProgStepResult newSubVals newOutput])
      go xs newSubVals (r ++ newOutput)

type DPCache = TreeCache Concrete.Val DPProgStepResult

runDPProgCached ::
  DPCache -> [Concrete.Val] -> DPProg -> AngelicContext ([Val], DPCache)
runDPProgCached cache inputs prog = do
  let (cachedResults, remainingInputs) = queryTreeCache cache inputs
  case cachedResults of
    [] -> do
      (res, allResults) <-
        runStateT
          ( runDPProgFromSub
              (dpSubInit prog)
              remainingInputs
              (dpSubRec prog)
              (dpFinal prog)
              []
          )
          []
      return (res, insertTreeCache cache (zip inputs allResults))
    l -> do
      let cachedOutputs = stepOutput =<< l
      let cachedSubs = stepSub $ last l
      (res, allResults) <-
        runStateT
          ( runDPProgFromSub
              cachedSubs
              remainingInputs
              (dpSubRec prog)
              (dpFinal prog)
              cachedOutputs
          )
          []
      return (res, insertTreeCache cache (zip inputs $ l ++ allResults))

runDPProgCached' ::
  DPCache ->
  [Concrete.Val] ->
  [Concrete.Val] ->
  DPProg ->
  FreshIdent ->
  (SymBool, DPCache)
runDPProgCached' cache inputs output prog ident = case x of
  [(b, Right (o, newCache))] ->
    (b &&~ o ==~ toSym output, newCache)
  _ -> undefined
  where
    y =
      toGuardedList $
        runExceptT $
          flip runFreshT ident $
            runDPProgCached cache inputs prog
    x = filter (isRight . snd) y

data SynthComponentCached = SynthComponentCached
  { synthComponentCachedMaxInputListLength :: Int,
    synthComponentCachedFuzzingSpec :: [Concrete.Val] -> [Concrete.Val],
    synthComponentCachedFuzzingGenerator :: Gen Concrete.Val
  }

instance
  CEGISAlgorithm
    SynthComponentCached
    Concrete.DPProgCex
    DPCache
    [InputGen]
    DPProg
    Concrete.DPProg
  where
  runVerifier problem gens prog = do
    r <-
      quickCheckDPProgMaximizeCaching
        (synthComponentCachedFuzzingSpec problem)
        (synthComponentCachedMaxInputListLength problem)
        gens
        prog
    case r of
      Nothing -> return (VerificationSuccess, [])
      Just (cex, gens') -> return (VerificationFoundCex cex, gens')
  initialSynthConditionState _ = emptyTreeCache
  initialVerifierState problem =
    [ InputGen i 10000 [] (synthComponentCachedFuzzingGenerator problem)
      | i <- [1 .. synthComponentCachedMaxInputListLength problem]
    ]
  synthCondition _ cache prog cex =
    runDPProgCached'
      cache
      (Concrete.dpProgCexInput cex)
      (Concrete.dpProgCexOutput cex)
      prog
