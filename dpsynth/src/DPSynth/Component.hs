{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module DPSynth.Component where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get)
import DPSynth.Concrete (quickCheckDPProg)
import qualified DPSynth.Concrete as Concrete
import DPSynth.Error
  ( DPSynthError
      ( BadNumOfOperands,
        BadProgInputs,
        BadSemantics,
        BadStmtOutputs,
        NotOptimal,
        UndefinedVar,
        UnknownOperator
      ),
  )
import DPSynth.Experimental.CEGIS
  ( CEGISAlgorithm (initialSynthConditionState, initialVerifierState, runVerifier, synthCondition),
    VerificationResult (VerificationFoundCex, VerificationSuccess),
  )
import DPSynth.Util.Arith (symMax)
import Data.Foldable (Foldable (foldl'))
import Data.List (tails)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    FreshT,
    GenSymSimple (simpleFresh),
    LogicalOp (implies, (&&~)),
    Mergeable,
    SEq ((/=~), (==~)),
    SOrd ((<=~), (<~), (>=~)),
    SimpleListSpec (SimpleListSpec),
    Solvable (con),
    SymBool,
    SymIntN,
    SymWordN,
    ToCon,
    ToSym (toSym),
    UnionM,
    mrgFmap,
    mrgReturn,
    mrgSequence_,
    mrgTraverse_,
    runFreshT,
    symAssertWith,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.State.Class (mrgModify)
import Test.QuickCheck.Counterexamples (Gen, vectorOf)

type VarId = SymWordN 8

type Val = SymIntN 8

data Stmt = Stmt {stmtOp :: T.Text, stmtIn :: [VarId], stmtOut :: VarId}
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym, SEq, SOrd) via Default Stmt

deriving via Default Concrete.Stmt instance ToCon Stmt Concrete.Stmt

data Prog = Prog
  { progInNum :: Int,
    progStmts :: [Stmt],
    progOutIds :: [VarId]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via Default Prog

deriving via Default Concrete.Prog instance ToCon Prog Concrete.Prog

symAll :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAll f = foldl' (\acc v -> acc &&~ f v) (con True)

inBound :: VarId -> VarId -> SymBool
inBound bound val = (0 <=~ val) &&~ (val <~ bound)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

data IdValPair = IdValPair {idValPairId :: VarId, idValPairVal :: Val}
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym, Mergeable) via Default IdValPair

data Env = Env
  { envIn :: [IdValPair],
    envOut :: [IdValPair]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym, Mergeable) via Default Env

type Context = ExceptT DPSynthError UnionM

type AngelicContext = FreshT Context

type EvalContext = StateT Env AngelicContext

addInputs :: [IdValPair] -> EvalContext ()
addInputs i = mrgModify (\env -> env {envIn = i ++ envIn env})

addOutput :: IdValPair -> EvalContext ()
addOutput o = mrgModify (\env -> env {envOut = o : envOut env})

addProgInVals :: [Val] -> EvalContext ()
addProgInVals inVals =
  mrgTraverse_ addOutput (zipWith IdValPair (con <$> [0 ..]) inVals)

genProgOutVals :: [VarId] -> EvalContext [Val]
genProgOutVals outIds = do
  outVals <- simpleFresh (SimpleListSpec (length outIds) ())
  addInputs $ zipWith IdValPair outIds outVals
  mrgReturn outVals

constrainStmt :: Int -> Stmt -> EvalContext ()
constrainStmt idBound (Stmt op inId outId) = do
  symAssertWith UndefinedVar $ symAll (inBound outId) inId
  symAssertWith BadStmtOutputs $ inBound (fromIntegral idBound) outId
  symAssertWith NotOptimal $ symAll (uncurry (<~)) $ zip inId (tail inId)

  inVal <- simpleFresh (SimpleListSpec (length inId) ())
  outVal <- simpleFresh ()

  addInputs $ zipWith IdValPair inId inVal
  addOutput $ IdValPair outId outVal

  case op of
    "zero" -> do
      symAssertWith BadSemantics $ outVal ==~ 0
    "max" -> do
      symAssertWith BadNumOfOperands $ length inVal >=~ 1
      symAssertWith BadSemantics $ outVal ==~ foldl1 symMax inVal
    "add" -> symAssertWith BadSemantics $ outVal ==~ sum inVal
    "uminus" -> do
      symAssertWith BadNumOfOperands $ length inVal >=~ 1
      symAssertWith BadSemantics $ outVal ==~ negate (head inVal)
    _ -> mrgThrowError $ UnknownOperator op

connected :: Env -> EvalContext ()
connected env =
  mrgSequence_ $
    [ symAssertWith BadSemantics $
        (idValPairId inPair ==~ idValPairId outPair)
          `implies` (idValPairVal inPair ==~ idValPairVal outPair)
      | inPair <- envIn env,
        outPair <- envOut env
    ]

outDistinct :: Env -> EvalContext ()
outDistinct =
  mrgTraverse_ (symAssertWith BadSemantics . uncurry (/=~))
    . pairs
    . fmap idValPairId
    . envOut

canonical :: [Stmt] -> EvalContext ()
canonical [] = mrgReturn ()
canonical (x : xs) = do
  go x xs
  canonical xs
  where
    ca stmt1 stmt2 =
      symAssertWith NotOptimal $
        ( stmtOut stmt1
            ==~ (stmtOut stmt2 + 1)
            &&~ symAll (/=~ stmtOut stmt2) (stmtIn stmt1)
        )
          `implies` (stmt2 <~ stmt1)
    go _ [] = mrgReturn ()
    go stmt1 (stmt2 : ss) = do
      ca stmt1 stmt2
      ca stmt2 stmt1
      go stmt1 ss

runProg :: Prog -> [Val] -> AngelicContext [Val]
runProg prog inVals = flip evalStateT (Env [] []) $ do
  symAssertWith BadProgInputs $ length inVals ==~ progInNum prog
  -- canonical $ progStmts prog
  addProgInVals inVals

  let bound = progInNum prog + length (progStmts prog)
  mrgTraverse_ (constrainStmt bound) $ progStmts prog

  outVals <- genProgOutVals $ progOutIds prog
  symAssertWith UndefinedVar $
    symAll (inBound (fromIntegral bound)) $
      progOutIds prog

  symAssertWith NotOptimal $
    symAll (uncurry (<~)) $
      zip (progOutIds prog) (tail $ progOutIds prog)

  connected =<< get
  outDistinct =<< get
  mrgReturn outVals

data DPProg = DPProg
  { dpSubInit :: [Val],
    dpSubRec :: Prog,
    dpFinal :: Prog
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via Default DPProg

deriving via (Default Concrete.DPProg) instance ToCon DPProg Concrete.DPProg

runDPProg :: DPProg -> [Val] -> AngelicContext [Val]
runDPProg prog input = go input (dpSubInit prog)
  where
    go [] _ = mrgReturn []
    go (x : xs) lastSub = do
      newSub <- runProg (dpSubRec prog) (x : lastSub)
      result <- runProg (dpFinal prog) newSub
      mrgFmap (result ++) $ go xs newSub

instance GenSymSimple (T.Text, Int) Stmt where
  simpleFresh (op, nin) =
    uncurry (Stmt op) <$> simpleFresh (SimpleListSpec nin (), ())

data ProgSpace = ProgSpace
  { progSpaceInNum :: Int,
    progSpaceStmtSpec :: [(T.Text, Int)],
    progSpaceOutNum :: Int
  }

instance GenSymSimple ProgSpace Prog where
  simpleFresh (ProgSpace numIn stmtSpec numOut) = do
    outIds <- simpleFresh (SimpleListSpec numOut ())
    stmts <- traverse simpleFresh stmtSpec
    return $ Prog numIn stmts outIds

data DPProgSpace = DPProgSpace
  { dpProgSpaceSubInitSpec :: [Concrete.Val],
    dpProgSpaceSubRecSpec :: ProgSpace,
    dpProgSpaceFinalSpec :: ProgSpace
  }

instance GenSymSimple DPProgSpace DPProg where
  simpleFresh (DPProgSpace subInit subRec final) = do
    uncurry (DPProg (toSym subInit)) <$> simpleFresh (subRec, final)

data SynthComponent = SynthComponent
  { synthComponentMaxInputListLength :: Int,
    synthComponentMaxNumTests :: Int,
    synthComponentFuzzingSpec :: [Concrete.Val] -> [Concrete.Val],
    synthComponentFuzzingGenerator :: Gen Concrete.Val
  }

instance
  CEGISAlgorithm
    SynthComponent
    Concrete.DPProgCex
    ()
    [Gen [Concrete.Val]]
    DPProg
    Concrete.DPProg
  where
  synthCondition _ _ prog cex ident =
    let result = runDPProg prog (toSym (Concrete.dpProgCexInput cex))
     in ( toSym
            ( return (Concrete.dpProgCexOutput cex) ::
                Context [Concrete.Val]
            )
            ==~ (runFreshT result ident :: Context [Val]),
          ()
        )
  runVerifier problem gens prog = do
    r <-
      quickCheckDPProg
        (synthComponentFuzzingSpec problem)
        (synthComponentMaxNumTests problem)
        gens
        prog
    case r of
      Nothing -> return (VerificationSuccess, [])
      Just (cex, gens') -> return (VerificationFoundCex cex, gens')
  initialSynthConditionState _ = ()
  initialVerifierState problem =
    [ vectorOf n (synthComponentFuzzingGenerator problem)
      | n <- [1 .. synthComponentMaxInputListLength problem]
    ]
