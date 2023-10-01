{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module DPSynth.ByteCode where

import Control.Monad (when, zipWithM)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, get)
import DPSynth.Concrete (quickCheckDPProg)
import qualified DPSynth.Concrete as Concrete
import DPSynth.Error
  ( DPSynthError
      ( BadNumOfOperands,
        BadProgInputs,
        BadStmtOutputs,
        UndefinedVar,
        UnknownOperator
      ),
  )
import DPSynth.Experimental.CEGIS
  ( CEGISAlgorithm (initialSynthConditionState, initialVerifierState, runVerifier, synthCondition),
    VerificationResult (VerificationFoundCex, VerificationSuccess),
  )
import DPSynth.Util.Arith (symMax)
import Data.List (sortOn)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    GenSymSimple (simpleFresh),
    SEq ((==~)),
    SimpleListSpec (SimpleListSpec),
    SymIntN,
    SymWordN,
    ToCon,
    ToSym (toSym),
    UnionM,
    chooseFresh,
    mrgFmap,
    mrgIf,
    mrgReturn,
    mrgTraverse,
    mrgTraverse_,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.Trans.State.Lazy (mrgEvalStateT, mrgModify)
import Test.QuickCheck (Gen, vectorOf)

type VarId = SymWordN 8

type Val = SymIntN 8

data Stmt = Stmt
  { stmtOp :: UnionM T.Text,
    stmtIn :: [VarId],
    stmtOut :: Concrete.VarId
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via Default Stmt

deriving via (Default Concrete.Stmt) instance ToCon Stmt Concrete.Stmt

data Prog = Prog
  { progInNum :: Int,
    progStmts :: [Stmt],
    progOutIds :: [VarId]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via Default Prog

deriving via (Default Concrete.Prog) instance ToCon Prog Concrete.Prog

type Env = [Val]

type Context = ExceptT DPSynthError UnionM

type EvalContext = StateT Env Context

getVal :: VarId -> EvalContext Val
getVal i = get >>= go i
  where
    go _ [] = mrgThrowError UndefinedVar
    go n (v : vs) = mrgIf (n ==~ 0) (return v) (go (n - 1) vs)

addVal :: Val -> EvalContext ()
addVal v = mrgModify (++ [v])

interpretStmt :: Stmt -> EvalContext ()
interpretStmt stmt = do
  inVals <- traverse getVal (stmtIn stmt)
  op <- lift $ lift $ stmtOp stmt
  case op of
    "zero" -> addVal 0
    "max" -> do
      when (null inVals) $ mrgThrowError BadNumOfOperands
      addVal $ foldl1 symMax inVals
    "add" -> addVal $ sum inVals
    "uminus" -> do
      when (null inVals) $ mrgThrowError BadNumOfOperands
      addVal $ negate $ head inVals
    _ -> mrgThrowError $ UnknownOperator op

runProg :: Prog -> [Val] -> Context [Val]
runProg prog inVals = do
  when (length inVals /= progInNum prog) $ mrgThrowError BadProgInputs
  let sortedStmts = sortOn stmtOut $ progStmts prog
  when
    ( fmap stmtOut sortedStmts
        /= take (length sortedStmts) [fromIntegral (progInNum prog) ..]
    )
    $ mrgThrowError BadStmtOutputs
  flip mrgEvalStateT inVals $ do
    mrgTraverse_ interpretStmt sortedStmts
    mrgTraverse getVal (progOutIds prog)

data DPProg = DPProg
  { dpSubInit :: [Val],
    dpSubRec :: Prog,
    dpFinal :: Prog
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via Default DPProg

deriving via (Default Concrete.DPProg) instance ToCon DPProg Concrete.DPProg

runDPProg :: DPProg -> [Val] -> Context [Val]
runDPProg prog input = go input (dpSubInit prog)
  where
    go [] _ = mrgReturn []
    go (x : xs) lastSub = do
      newSub <- runProg (dpSubRec prog) (x : lastSub)
      result <- runProg (dpFinal prog) newSub
      mrgFmap (result ++) $ go xs newSub

data ProgSpace = ProgSpace
  { progSpaceInNum :: Int,
    progSpaceStmtSpec :: [([T.Text], Int)],
    progSpaceOutNum :: Int
  }

instance GenSymSimple ProgSpace Prog where
  simpleFresh (ProgSpace numIn stmtSpec numOut) = do
    outIds <- simpleFresh (SimpleListSpec numOut ())
    stmts <-
      zipWithM
        ( \outId (ops, maxInput) -> do
            op <- chooseFresh ops
            inputs <- simpleFresh (SimpleListSpec maxInput ())
            return $ Stmt op inputs outId
        )
        [fromIntegral numIn ..]
        stmtSpec
    return $ Prog numIn stmts outIds

data DPProgSpace = DPProgSpace
  { dpProgSpaceSubInitSpec :: [Concrete.Val],
    dpProgSpaceSubRecSpec :: ProgSpace,
    dpProgSpaceFinalSpec :: ProgSpace
  }

instance GenSymSimple DPProgSpace DPProg where
  simpleFresh (DPProgSpace subInit subRec final) =
    uncurry (DPProg (toSym subInit)) <$> simpleFresh (subRec, final)

data SynthByteCode = SynthByteCode
  { synthByteCodeMaxInputListLength :: Int,
    synthByteCodeMaxNumTests :: Int,
    synthByteCodeFuzzingSpec :: [Concrete.Val] -> [Concrete.Val],
    synthByteCodeFuzzingGenerator :: Gen Concrete.Val
  }

instance
  CEGISAlgorithm
    SynthByteCode
    Concrete.DPProgCex
    ()
    [Gen [Concrete.Val]]
    DPProg
    Concrete.DPProg
  where
  synthCondition _ _ prog cex _ =
    let result = runDPProg prog (toSym (Concrete.dpProgCexInput cex))
     in ( toSym
            ( return (Concrete.dpProgCexOutput cex) ::
                Context [Concrete.Val]
            )
            ==~ (result :: Context [Val]),
          ()
        )
  runVerifier problem gens prog = do
    r <-
      quickCheckDPProg
        (synthByteCodeFuzzingSpec problem)
        (synthByteCodeMaxNumTests problem)
        gens
        prog
    case r of
      Nothing -> return (VerificationSuccess, [])
      Just (cex, gens') -> return (VerificationFoundCex cex, gens')
  initialSynthConditionState _ = ()
  initialVerifierState problem =
    [ vectorOf n (synthByteCodeFuzzingGenerator problem)
      | n <- [1 .. synthByteCodeMaxInputListLength problem]
    ]
