{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Builder
  ( OpBuilder (..),
    ProgBuilder (..),
    StmtBuilder (..),
  )
import Data.Bits (Bits (xor))
import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    SOrd ((.<=)),
    UnionM,
    boolector,
    chooseUnionFresh,
    mrgIte,
    precise,
    runFresh,
  )
import Program
  ( Op,
    Prog (..),
    Stmt (Stmt),
    SymVal,
    SymVarId,
  )
import Synthesize (synthesize)
import Verify (verify)

bitXorSpec :: [SymVal] -> SymVal
bitXorSpec [a, b] = xor a b
bitXorSpec _ = undefined

-- void bitXor(int a, int b) {
--   int v2 = ~a;
--   int v3 = ~b;
--   int v4 = a & b;
--   int v5 = v2 & v3;
--   int v6 = ~v4;
--   int v7 = ~v5;
--   int v8 = v6 & v7;
--   return v8;
-- }
bitXorProg :: Prog
bitXorProg =
  Prog
    2
    [ Stmt mkBitNot [0],
      Stmt mkBitNot [1],
      Stmt mkBitAnd [0, 1],
      Stmt mkBitAnd [2, 3],
      Stmt mkBitNot [4],
      Stmt mkBitNot [5],
      Stmt mkBitAnd [6, 7]
    ]

isLessOrEqualSpec :: [SymVal] -> SymVal
isLessOrEqualSpec [a, b] = mrgIte (a .<= b) 1 0
isLessOrEqualSpec _ = undefined

-- void isLessOrEqual(int a, int b) {
--   int v2 = ~a;
--   int v3 = 1;
--   int v4 = v2 + v3;  // -a
--   int v5 = v4 + v3;  // b - a, bit 32: b - a is negative
--   int v6 = ~b;
--   int v7 = a & v6;   // bit 32: a is negative and b is non-negative
--   int v8 = a ^ b;    // bit 32: a and b have different signs
--   int v9 = ~v8;      // bit 32: a and b have the same sign
--   int v10 = ~v5;     // bit 32: b - a is non-negative
--   int v11 = v8 & v9; // bit 32: a and b have the same sign and b - a is non-negative
--   int v12 = v7 | v11;
--   int v13 = 31;
--   int v14 = v12 >> v13;
--   int v15 = v14 & v3;
--   return v15;
-- }
isLessOrEqualProg :: Prog
isLessOrEqualProg =
  Prog
    2
    [ Stmt mkBitNot [0],
      Stmt (mkLit 1) [],
      Stmt mkPlus [2, 3],
      Stmt mkPlus [4, 1],
      Stmt mkBitNot [1],
      Stmt mkBitAnd [0, 6],
      Stmt mkBitXor [0, 1],
      Stmt mkBitNot [8],
      Stmt mkBitNot [5],
      Stmt mkBitAnd [9, 10],
      Stmt mkBitOr [7, 11],
      Stmt (mkLit 31) [],
      Stmt mkRightShift [12, 13],
      Stmt mkBitAnd [14, 3]
    ]

freshArgList :: [Fresh SymVarId]
freshArgList = [simpleFresh (), simpleFresh ()]

notOrAnd :: Fresh (UnionM Op)
notOrAnd = chooseUnionFresh [mkBitNot, mkBitAnd]

bitXorProgSketch :: Prog
bitXorProgSketch =
  flip runFresh "prog" $
    mkProg
      2
      [ mkStmt notOrAnd freshArgList,
        mkStmt notOrAnd freshArgList,
        mkStmt notOrAnd freshArgList,
        mkStmt notOrAnd freshArgList,
        mkStmt notOrAnd freshArgList,
        mkStmt notOrAnd freshArgList,
        mkStmt notOrAnd freshArgList
      ]

main :: IO ()
main = do
  let config = precise boolector
  xorCex <- verify config bitXorSpec bitXorProg
  print xorCex
  isLessOrEqualCex <- verify config isLessOrEqualSpec isLessOrEqualProg
  print isLessOrEqualCex

  synthesized <- synthesize config bitXorSpec bitXorProgSketch
  print synthesized
