{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | Module      : Program
--   Description : The definition of a small DSL for bit-level programs.
--   Copyright   : (c) Sirui Lu, 2023
--   License     : BSD3
--   Maintainer  : siruilu@cs.washington.edu
--   Stability   : experimental
--
-- This module defines a small DSL for bit-level programs and provides the
-- interpreter for the DSL that translate the program into symbolic formulas.
module Program
  ( -- * The program representation

    -- |
    --
    -- Our program is represented as a sequence of statements. Each statement
    -- has one operator and zero or more input variables:
    --
    -- > int bitOr(int a, int b) {
    -- >   int v2 = ~a;
    -- >   int v3 = ~b;
    -- >   int v4 = v2 & v3;
    -- >   int v5 = ~v4;
    -- >   return v5
    -- > }
    --
    -- The above program is represented as follows (the following code does not
    -- type check, but it gives you an idea of how the program is represented):
    --
    -- > Prog 2
    -- >   [ Stmt BitNot [0]
    -- >   , Stmt BitNot [1]
    -- >   , Stmt BitAnd [2, 3]
    -- >   , Stmt BitNot [4]
    -- >   ]
    --
    -- The 2 in the first line indicates that the program has two input
    -- variables. The variables in the program are indexed from 0. In this
    -- example, the two inputs (@a@ and @b@) are indexed as 0 and 1,
    -- respectively, and the statements are defining the variable 2 to 5. Note
    -- that no variable could be redefined. The statements in the program are
    -- executed sequentially, and could refer to any defined variable. The last
    -- variable (i.e., 5 in this variable), is the return value of the program.
    --
    -- If the supplied input variables to a statement is more than the number of
    -- expected input variables, the program is still considered as well-formed,
    -- and the extra input variables are ignored. Statements with insufficient
    -- input variables are considered ill-formed.

    -- * The operators in the DSL

    -- |
    -- * @'Lit' v@: a literal value @v@.
    -- * @'Not'@: C's unary @!@ operator, the result is 1 when the operand is 0,
    --   or the result is 1.
    -- * @'BitNot'@: C's unary @~@ operator, the result is the bitwise negation
    --   of the operand.
    -- * @'BitAnd'@: C's binary @&@ operator, the result is the bitwise and of
    --   the two operands.
    -- * @'BitOr'@: C's binary @|@ operator, the result is the bitwise or of the
    --   two operands.
    -- * @'BitXor'@: C's binary @^@ operator, the result is the bitwise xor of
    --   the two operands.
    -- * @'Plus'@: C's binary @+@ operator, the result is the addition of the
    --   two operands.
    -- * @'LeftShift'@: C's binary @<<@ operator, the result is the left shift
    --   of the first operand by the second operand. Shifting by a negative
    --   number is undefined behavior.
    -- * @'RightShift'@: C's binary @>>@ operator, the result is the right shift
    --   of the first operand by the second operand. Shifting by a negative
    --   number is undefined behavior.

    -- * Program sketches

    -- | To do program synthesis, using a program sketch is one of the common
    -- approaches. A program sketch is a program with some holes that could be
    -- chosen by the solvers.
    --
    -- For example, the following can be a program sketch for our @bitOr@
    -- example:
    --
    -- > Prog 2
    -- >   [ Stmt BitNot [0]
    -- >   , Stmt BitNot [1]
    -- >   , Stmt choose{BitNot, BitAnd} [??, ??]
    -- >   , Stmt BitNot [4]
    -- >   ]
    --
    -- We extend the syntax of the program representation to support program
    -- the following two new constructs:
    --
    -- * @'choose' {op1, op2, ...}@: a choice between the operators @op1@,
    --   @op2@, etc. The choice is made by the solver, and
    -- * @'??'@: a hole that could be filled by a variable. The solver is free
    --   to choose any defined variable to fill the hole, and here the choices
    --   could be made from 0, 1, 2, and 3.
    --
    -- The program sketch defines a program space. For example, the following
    -- two programs are in the space:
    --
    -- > Prog 2
    -- >   [ Stmt BitNot [0]
    -- >   , Stmt BitNot [1]
    -- >   , Stmt BitNot [2, 3] -- This is still ok, the extra input is ignored
    -- >   , Stmt BitNot [4]
    -- >   ]
    --
    -- > Prog 2
    -- >   [ Stmt BitNot [0]
    -- >   , Stmt BitNot [1]
    -- >   , Stmt BitAnd [2, 3]
    -- >   , Stmt BitNot [4]
    -- >   ]

    -- * The interpreter

    -- | To do symbolic reasoning about the program, we need to transform the
    -- program into symbolic formulas. This is made simple by Grisette. With
    -- Grisette, you can simply write an interpreter, and you will get a
    -- symbolic compiler.
    --
    -- The interpreter takes a program and a list of symbolic values as the
    -- input, and returns a symbolic value as the output. The output would be a
    -- formula with respect to the input and the symbolic choices made by the
    -- program.
    --
    -- In addition to this formula, the interpreter will also track the errors.
    -- It will exclude all the programs that is using undefined variables, or
    -- cause undefined behaviors. The condition for them will be encoded as SMT
    -- constraints.

    -- * APIs
    Error (..),
    Op (..),
    Stmt (..),
    Prog (..),
    SymVarId,
    SymVal,
    badProg,
    interpretProg,
  )
where

import Control.Monad.Except (ExceptT, MonadError, MonadTrans (lift))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Data.Bits (Bits (..))
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    Mergeable,
    SEq ((.==)),
    SymIntN,
    SymWordN,
    ToCon,
    UnionLike,
    UnionM,
    mrgIf,
    mrgReturn,
  )
import Grisette.Core.Data.Class.SafeSymShift
  ( SafeSymShift (safeSymShiftL', safeSymShiftR'),
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)

-- | We use symbolic unsigned 8-bit integers as the variable identifiers.
type SymVarId = SymWordN 8

-- | We use symbolic signed 32-bit integers as the values, as in the CS:APP lab.
type SymVal = SymIntN 32

-- | The error that could be raised by the interpreter.
data Error
  = -- | For undefined variables and unmatched number of input variables.
    BadProg
  | -- | Undefined behaviors.
    UndefinedBehavior
  deriving (Eq, Show, Generic)
  -- We derive these type classes to make it compatible with the Grisette
  -- library.
  deriving (ToCon Error, Mergeable, SEq) via (Default Error)

-- | The operators in the DSL.
data Op
  = Lit SymVal
  | Not
  | BitNot
  | BitAnd
  | BitOr
  | BitXor
  | Plus
  | LeftShift
  | RightShift
  deriving (Show, Eq, Generic)
  deriving (Mergeable, EvaluateSym) via (Default Op)

-- | The statements in the program.
data Stmt = Stmt
  { -- | The operator of the statement. Here we use the `UnionM` data structure
    -- to represent the operator and their choices.
    stmtOp :: UnionM Op,
    stmtInId :: [SymVarId]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default Stmt)

-- | The program representation.
data Prog = Prog {progInNum :: Int, progStmts :: [Stmt]}
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default Prog)

type Context = ExceptT Error UnionM

type StateContext = StateT [SymVal] Context

result :: (Mergeable a, UnionLike m, MonadError Error m) => a -> m a
result = mrgReturn

-- | Throw an `BadProg` error.
badProg :: (Mergeable a, UnionLike m, MonadError Error m) => m a
badProg = mrgThrowError BadProg

useVar :: SymVarId -> StateContext SymVal
useVar varId = do
  env <- get
  go env (fromIntegral (length env) - 1 - varId)
  where
    go [] _ = badProg
    go (x : xs) v = mrgIf (v .== 0) (result x) (go xs (v - 1))

defVar :: SymVal -> StateContext ()
defVar val = do
  env <- get
  put $ val : env

unaryOp :: (SymVal -> StateContext SymVal) -> [SymVal] -> StateContext SymVal
unaryOp _ l | null l = badProg
unaryOp f l = f $ head l

binaryOp ::
  (SymVal -> SymVal -> StateContext SymVal) -> [SymVal] -> StateContext SymVal
binaryOp _ l | length l < 2 = badProg
binaryOp f l = f (head l) (l !! 1)

-- | The program interpreter. Translates a program into a symbolic formula.
interpretProg :: [SymVal] -> Prog -> Context SymVal
interpretProg inputs (Prog num _) | length inputs /= num = badProg
interpretProg inputs (Prog _ stmts') =
  evalStateT (go stmts') (reverse inputs)
  where
    go [] = do
      env <- get
      mrgReturn $ head env
    go (Stmt op inIds : stmts) = do
      vals <- traverse useVar inIds
      singleOp <- lift $ lift op
      res <- case singleOp of
        Lit v -> mrgReturn v
        Not -> unaryOp (\v -> mrgIf (v .== 0) (result 1) (result 0)) vals
        BitNot -> unaryOp (return . complement) vals
        BitAnd -> binaryOp (\a b -> return $ a .&. b) vals
        BitOr -> binaryOp (\a b -> return $ a .|. b) vals
        BitXor -> binaryOp (\a b -> return $ a `xor` b) vals
        Plus -> binaryOp (\a b -> return $ a + b) vals
        LeftShift -> binaryOp (safeSymShiftL' (const UndefinedBehavior)) vals
        RightShift -> binaryOp (safeSymShiftR' (const UndefinedBehavior)) vals
      defVar res
      go stmts
