{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Module      : Builder
--   Description : Smart constructors for the DSL for bit-level programs.
--   Copyright   : (c) Sirui Lu, 2023
--   License     : BSD3
--   Maintainer  : siruilu@cs.washington.edu
--   Stability   : experimental
--
-- This module provides smart constructors for the DSL for bit-level programs.
-- We consider this as boilerplate code and will provide Template Haskell
-- support to generate this code in the future.
module Builder
  ( OpBuilder (..),
    StmtBuilder (..),
    ProgBuilder (..),
  )
where

import Grisette (Fresh, UnionM, mrgReturn)
import Program (Op (..), Prog (Prog), Stmt (Stmt), SymVal, SymVarId)

-- | We use the `OpBuilder` to build 'Op', @'UnionM' 'Op'@ and
-- @'Fresh' ('UnionM' 'Op')@
class OpBuilder op where
  mkLit :: SymVal -> op
  mkNot :: op
  mkBitNot :: op
  mkBitAnd :: op
  mkBitOr :: op
  mkBitXor :: op
  mkPlus :: op
  mkLeftShift :: op
  mkRightShift :: op

instance OpBuilder Op where
  mkLit = Lit
  mkNot = Not
  mkBitNot = BitNot
  mkBitAnd = BitAnd
  mkBitOr = BitOr
  mkBitXor = BitXor
  mkPlus = Plus
  mkLeftShift = LeftShift
  mkRightShift = RightShift

instance OpBuilder (UnionM Op) where
  mkLit = mrgReturn . Lit
  mkNot = mrgReturn Not
  mkBitNot = mrgReturn BitNot
  mkBitAnd = mrgReturn BitAnd
  mkBitOr = mrgReturn BitOr
  mkBitXor = mrgReturn BitXor
  mkPlus = mrgReturn Plus
  mkLeftShift = mrgReturn LeftShift
  mkRightShift = mrgReturn RightShift

instance OpBuilder (Fresh (UnionM Op)) where
  mkLit = return . mkLit
  mkNot = return mkNot
  mkBitNot = return mkBitNot
  mkBitAnd = return mkBitAnd
  mkBitOr = return mkBitOr
  mkBitXor = return mkBitXor
  mkPlus = return mkPlus
  mkLeftShift = return mkLeftShift
  mkRightShift = return mkRightShift

-- | We use the `StmtBuilder` to build 'Stmt' and @'Fresh' 'Stmt'@
class StmtBuilder stmt op varId | stmt -> op varId where
  mkStmt :: op -> [varId] -> stmt

instance StmtBuilder Stmt (UnionM Op) SymVarId where
  mkStmt = Stmt

instance StmtBuilder (Fresh Stmt) (Fresh (UnionM Op)) (Fresh SymVarId) where
  mkStmt op varIds = mkStmt <$> op <*> sequence varIds

-- | We use the `StmtBuilder` to build 'Stmt' and @'Fresh' 'Prog'@
class ProgBuilder prog stmt | prog -> stmt where
  mkProg :: Int -> [stmt] -> prog

instance ProgBuilder Prog Stmt where
  mkProg = Prog

instance ProgBuilder (Fresh Prog) (Fresh Stmt) where
  mkProg i stmts = Prog i <$> sequence stmts
