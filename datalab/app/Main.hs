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
    LogicalOp (..),
    SOrd ((<=~)),
    UnionM,
    boolector,
    chooseUnionFresh,
    mrgIte,
    precise,
    runFresh,
  )
import Grisette.Core (SOrd (..))
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
isLessOrEqualSpec [a, b] = mrgIte (a <=~ b) 1 0
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

{-
def isLessThanOrEqual(a, b):
    asign = getSign(a) # 0 if a is non-negative, 1 if a is negative
    print("asign", asign)
    bsign = getSign(b) # 0 if b is non-negative, 1 if b is negative
    print("bsign", bsign)
    bmasign = subtractAndCheckSign(b, a) # 0 if b >= a, 1 if b < a
    print("bmasign", bmasign)
    notbmasign = np.bitwise_not(bmasign) # 1 if b >= a, 0 if b < a
    print("notbmasign", notbmasign)
    diffsign = np.bitwise_xor(asign, bsign) # 1 if a and b have different signs, 0 if a and b have the same sign
    print("diffsign", diffsign)
    anegbnonneg = np.bitwise_and(asign, diffsign)
    print("anegbnonneg", anegbnonneg)
    samesign = np.bitwise_not(diffsign)
    print("samesign", samesign)
    r9 = np.bitwise_and(notbmasign, samesign)
    print("r9", r9)
    r10 = np.bitwise_or(anegbnonneg, r9)
    print("r10", r10)
    r11 = 1
    r12 = np.bitwise_and(r10, r11)
    return r12
-}

isLessOrEqualProg' :: Prog
isLessOrEqualProg' =
  Prog
    2
    [ Stmt mkGetSign [0], -- 2
      Stmt mkGetSign [1], -- 3
      Stmt mkSubtractAndCheckSign [1, 0], -- 4
      Stmt mkBitNot [4], -- 5: 1 if a <= b is negative
      Stmt mkBitXor [2, 3], -- 6: 1 if a and b have different signs
      Stmt mkBitAnd [2, 6], -- a <= 0 && b > 0
      Stmt mkBitNot [6], -- same signs
      Stmt mkBitAnd [5, 8],
      Stmt mkBitOr [7, 9],
      Stmt (mkLit 1) [],
      Stmt mkBitAnd [10, 11]
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

notOrAndOr :: Fresh (UnionM Op)
notOrAndOr = chooseUnionFresh [mkBitNot, mkBitAnd, mkBitOr]

isLessOrEqualProgSketch :: Prog
isLessOrEqualProgSketch =
  flip runFresh "prog" $
    mkProg
      2
      [ mkStmt mkSignDifference [return 0, return 1],
        mkStmt mkSubtractAndCheckSign freshArgList,
        mkStmt mkGetSign [simpleFresh ()],
        mkStmt notOrAnd [simpleFresh (), simpleFresh ()],
        mkStmt notOrAnd [simpleFresh (), simpleFresh ()],
        mkStmt andOr [simpleFresh (), simpleFresh ()],
        mkStmt andOr [simpleFresh (), simpleFresh ()],
        mkStmt andOr [simpleFresh (), simpleFresh ()],
        mkStmt (mkLit 1) [],
        mkStmt mkBitAnd [simpleFresh (), simpleFresh ()]
      ]

absSpec :: [SymVal] -> SymVal
absSpec [a] = mrgIte (a <=~ 0) (-a) a
absSpec _ = undefined

absSketch :: Prog
absSketch =
  flip runFresh "prog" $
    mkProg
      1
      [ mkStmt mkGetSign [return 0],
        mkStmt notOrAndOr freshArgList,
        mkStmt notOrAndOr freshArgList,
        mkStmt notOrAndOr freshArgList,
        mkStmt notOrAndOr freshArgList
      ]

compareSignsSpec :: [SymVal] -> SymVal
compareSignsSpec [a, b] =
  mrgIte (a <=~ 0) (mrgIte (b <=~ 0) 1 0) (mrgIte (b <=~ 0) 0 1)
compareSignsSpec _ = undefined

andOr :: Fresh (UnionM Op)
andOr = chooseUnionFresh [mkBitAnd, mkBitOr]

compareSignsSketch :: Prog
compareSignsSketch =
  flip runFresh "prog" $
    mkProg
      2
      [ mkStmt mkGetSign [return 0],
        mkStmt mkGetSign [return 1],
        mkStmt andOr freshArgList,
        mkStmt andOr freshArgList,
        mkStmt andOr freshArgList
      ]

subtractAndCheckSpec :: [SymVal] -> SymVal
subtractAndCheckSpec [a, b] = mrgIte (a - b >=~ 0) 1 0

subtractAndCheckSpecSketch :: Prog
subtractAndCheckSpecSketch =
  flip runFresh "prog" $
    mkProg
      2
      [ mkStmt (chooseUnionFresh [mkBitNot, mkPlus, mkGetSign]) freshArgList,
        mkStmt (chooseUnionFresh [mkBitNot, mkPlus, mkGetSign]) freshArgList,
        mkStmt (chooseUnionFresh [mkBitNot, mkPlus, mkGetSign]) freshArgList
      ]

main :: IO ()
main = do
  let config = precise boolector
  putStrLn "-- xor --"
  xorCex <- verify config bitXorSpec bitXorProg
  print xorCex
  synthesized <- synthesize config bitXorSpec bitXorProgSketch
  print synthesized

  putStrLn "-- isLessOrEqual --"
  isLessOrEqualCex <- verify config isLessOrEqualSpec isLessOrEqualProg
  print isLessOrEqualCex
  isLessOrEqualCex' <- verify config isLessOrEqualSpec isLessOrEqualProg'
  print isLessOrEqualCex'
  p' <- synthesize config subtractAndCheckSpec subtractAndCheckSpecSketch
  print p'
  p' <- synthesize config isLessOrEqualSpec isLessOrEqualProgSketch
  print p'
