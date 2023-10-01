{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module: DPSynth.Concrete
--
-- This module provides the definition of concrete dymamic programming programs
-- and the interpreter for them.
module DPSynth.Concrete
  ( VarId,
    Val,
    Stmt (..),
    Prog (..),
    Env,
    Context,
    EvalContext,
    getVal,
    addVal,
    interpretStmt,
    runProg,
    DPProg (..),
    runDPProg,
    DPProgCex (..),
    quickCheckDPProg,
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import DPSynth.Error
  ( DPSynthError
      ( BadNumOfOperands,
        BadProgInputs,
        BadStmtOutputs,
        UndefinedVar,
        UnknownOperator
      ),
  )
import Data.Foldable (traverse_)
import Data.List (sortOn)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (IntN, WordN)
import Test.QuickCheck.Counterexamples
  ( Args (chatty, maxSuccess),
    Gen,
    forAll,
    quickCheckWith,
    stdArgs,
    type (:&:) ((:&:)),
  )

-- | The variable ID type. In our program representation, each variable is
-- represented as an ID, which is a natural number. The ID is used to refer to
-- the variable in the program. Here we use a 8-bit unsigned integer for the
-- ID, this works when the number of variables in a program is less than 256.
type VarId = WordN 8

-- | The value type. Our dynamic programming program processes 8-bit signed
-- integers.
type Val = IntN 8

-- | A program statement. A program statement has an operator, a list of input
-- variables, and an output variable.
data Stmt = Stmt {stmtOp :: T.Text, stmtIn :: [VarId], stmtOut :: VarId}
  deriving (Show, Eq, Generic)

-- | A concrete program for the sub-problems. A program has a number of input
-- variables, a list of statements, and a list of output variables.
--
-- The following shows an example program:
--
-- > (input0, input1) => {
-- >   v2 = add(input0, input1)
-- >   v3 = max(input0, input1, v2)
-- >   return (v2, v3)
-- > }
--
-- The example program is represented as follows:
--
-- > Prog
-- >   { progInNum = 2,
-- >     progStmts =
-- >       [ Stmt {stmtOp = "add", stmtIn = [0, 1], stmtOut = 2},
-- >         Stmt {stmtOp = "max", stmtIn = [0, 1, 2], stmtOut = 3}
-- >       ],
-- >     progOutIds = [2, 3]
-- >   }
--
-- We require that the program inputs and the statement outputs are
-- consecutively numbered from 0.
data Prog = Prog {progInNum :: Int, progStmts :: [Stmt], progOutIds :: [VarId]}
  deriving (Show, Eq, Generic)

-- | The environment type. The environment is a list of values, where the
-- position of a value in the list is the variable ID.
type Env = [Val]

-- | The context type. The context is an error monad. The error monad is used
-- to represent the errors that may occur during the evaluation.
type Context = Either DPSynthError

-- | The evaluation context type. The evaluation context is a state monad
-- transformer over the environment and the error monad. The environment is
-- used to store the values of the variables. The error monad is used to
-- represent the errors that may occur during the evaluation.
type EvalContext = StateT Env Context

-- | Get the value of a variable from the environment.
-- If the variable ID is out of range, an error is thrown.
getVal :: VarId -> EvalContext Val
getVal i = get >>= go
  where
    go :: Env -> EvalContext Val
    go l
      | i < 0 || i >= fromIntegral (length l) =
          throwError UndefinedVar
    go l = return $ l !! fromIntegral i

-- | Add a value to the environment.
addVal :: Val -> EvalContext ()
addVal v = modify (++ [v])

-- | Interpret a statement. The statement is evaluated and the result is added
-- to the environment.
--
-- Four operators are supported:
--
-- * @zero@: add 0 to the environment.
-- * @max@: add the maximum of the input values to the environment.
-- * @add@: add the sum of the input values to the environment.
-- * @uminus@: add the negation of the first input value to the environment.
--
-- If the statement operator is not recognized, an error is thrown.
--
-- You may provide extra input values to the statement. These values are
-- ignored, but if you provide fewer input values than the statement requires,
-- an error is thrown.
interpretStmt :: Stmt -> EvalContext ()
interpretStmt stmt = do
  inVals <- traverse getVal (stmtIn stmt)
  case stmtOp stmt of
    "zero" -> addVal 0
    "max" -> do
      when (null inVals) $ throwError BadNumOfOperands
      addVal $ maximum inVals
    "add" -> addVal $ sum inVals
    "uminus" -> do
      when (null inVals) $ throwError BadNumOfOperands
      addVal $ negate $ head inVals
    op -> throwError $ UnknownOperator op

-- | Run a program.
--
-- An error will be thrown if the number of input values does not match the
-- number of program inputs, or if the program inputs and the statement outputs
-- are not consecutively numbered from 0.
runProg :: Prog -> [Val] -> Either DPSynthError [Val]
runProg prog inVals = do
  when (length inVals /= progInNum prog) $ throwError BadProgInputs
  let sortedStmts = sortOn stmtOut $ progStmts prog
  when
    ( fmap stmtOut sortedStmts
        /= take (length sortedStmts) [fromIntegral (progInNum prog) ..]
    )
    $ throwError BadStmtOutputs
  flip evalStateT inVals $ do
    traverse_ interpretStmt sortedStmts
    traverse getVal $ progOutIds prog

-- | A dynamic programming program. A dynamic programming program consists of
-- three parts:
--
-- * @dpSubInit@: the initial values of the sub-problems.
-- * @dpSubRec@: the program for the sub-problems.
-- * @dpFinal@: the program for the final problem.
--
-- The following shows an example dynamic programming program:
--
-- > def mss(xs):
-- >   # Initial values of the sub-problems.
-- >   suffix = 0;
-- >   best = 0;
-- >   for x in xs:
-- >     # Program for the sub-problems.
-- >     zero = 0;
-- >     pos_suffix_or_zero = max(suffix, zero);
-- >     new_suffix = x + pos_suffix_or_zero;
-- >     new_best = max(best, suffix);
-- >     suffix = new_suffix;
-- >     best = new_best;
-- >   # Program for the final problem.
-- >   ret = max(suffix, best);
-- >   return ret;
--
-- The example program is represented as follows:
--
-- > DPProg
-- >   { dpSubInit = [0, 0],  -- input0: suffix, input1: best
-- >     dpSubRec =
-- >       Prog
-- >         { progInNum = 3, -- input0: x, input1: suffix, input2: best
-- >           progStmts =
-- >             [ -- zero = 0;
-- >               Stmt {stmtOp = "zero", stmtIn = [], stmtOut = 3},
-- >               -- pos_suffix_or_zero = max(suffix, zero);
-- >               Stmt {stmtOp = "max", stmtIn = [1, 3], stmtOut = 4},
-- >               -- new_suffix = x + pos_suffix_or_zero;
-- >               Stmt {stmtOp = "add", stmtIn = [0, 4], stmtOut = 5},
-- >               -- new_best = max(best, suffix);
-- >               Stmt {stmtOp = "max", stmtIn = [2, 1], stmtOut = 6},
-- >             ],
-- >           progOutIds = [5, 6]
-- >         },
-- >     dpFinal =
-- >       Prog
-- >         { progInNum = 2, -- input0: suffix, input1: best
-- >           progStmts =
-- >             [ -- ret = max(suffix, best);
-- >               Stmt {stmtOp = "max", stmtIn = [0, 1], stmtOut = 2}
-- >             ],
-- >           progOutIds = [2]
-- >         }
-- >   }
data DPProg = DPProg
  { dpSubInit :: [Val],
    dpSubRec :: Prog,
    dpFinal :: Prog
  }
  deriving (Show, Eq, Generic)

-- | Run a dynamic programming program.
--
-- The function not only returns the final result, but also returns the dynamic
-- programming result for all suffixes of the input.
runDPProg :: DPProg -> [Val] -> Either DPSynthError [Val]
runDPProg prog input = go input (dpSubInit prog)
  where
    go [] _ = return []
    go (x : xs) lastSub = do
      newSub <- runProg (dpSubRec prog) (x : lastSub)
      result <- runProg (dpFinal prog) newSub
      (result ++) <$> go xs newSub

data DPProgCex = DPProgCex
  { dpProgCexInput :: [Val],
    dpProgCexOutput :: [Val]
  }
  deriving (Show, Eq, Generic)

quickCheckDPProg ::
  ([Val] -> [Val]) ->
  Int ->
  [Gen [Val]] ->
  DPProg ->
  IO (Maybe (DPProgCex, [Gen [Val]]))
quickCheckDPProg _ _ [] _ = return Nothing
quickCheckDPProg spec numOfTest (gen : gens) prog = do
  r <- quickCheckWith stdArgs {chatty = False, maxSuccess = numOfTest} $
    forAll gen $
      \input -> runDPProg prog input == Right (spec input)
  case r of
    Nothing -> quickCheckDPProg spec numOfTest gens prog
    Just (cex :&: ()) -> return $ Just (DPProgCex cex (spec cex), gen : gens)
