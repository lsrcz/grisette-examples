{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM)
import qualified DPSynth.ByteCode as ByteCode
import qualified DPSynth.Component as Component
import qualified DPSynth.Concrete as Concrete
import DPSynth.Util.MainFunc (mainFunc)
import Grisette (genSymSimple)
import Test.QuickCheck (Gen, chooseEnum)

-- Concrete programs and specs
mmmProg :: Concrete.DPProg
mmmProg =
  Concrete.DPProg
    [0, 0, 0]
    ( Concrete.Prog
        4
        [ Concrete.Stmt "uminus" [0] 4,
          Concrete.Stmt "add" [0, 1] 5,
          Concrete.Stmt "add" [0, 3] 6,
          Concrete.Stmt "add" [1, 4] 7,
          Concrete.Stmt "add" [2, 4] 8,
          Concrete.Stmt "max" [2, 3] 9,
          Concrete.Stmt "max" [5, 6] 10,
          Concrete.Stmt "max" [7, 8] 11
        ]
        [9, 10, 11]
    )
    (Concrete.Prog 3 [Concrete.Stmt "max" [0, 1, 2] 3] [3])

mmmSpec :: [Concrete.Val] -> [Concrete.Val]
mmmSpec l = [mmmSpec0 (take n l) | n <- [1 .. length l]]

mmmSpec0 :: [Concrete.Val] -> Concrete.Val
mmmSpec0 l = maximum $ fmap (sum . zipWith (*) l) allLists
  where
    allLists = filter isNotConsecutive $ replicateM (length l) [-1, 0, 1]
    isNotConsecutive [] = True
    isNotConsecutive [_] = True
    isNotConsecutive (a : b : _) | a == b = False
    isNotConsecutive (_ : x : xs) = isNotConsecutive $ x : xs

elementGen :: Gen Concrete.Val
elementGen = chooseEnum (-16, 16)

-- ByteCode
mmmByteCodeSpace :: ByteCode.DPProgSpace
mmmByteCodeSpace =
  ByteCode.DPProgSpace
    [0, 0, 0]
    ( ByteCode.ProgSpace
        4
        [ (["uminus"], 1),
          (["add"], 2),
          (["add"], 2),
          (["add"], 2),
          (["max"], 2),
          (["max"], 2),
          (["add", "max"], 2),
          (["add", "max"], 2)
        ]
        3
    )
    (ByteCode.ProgSpace 3 [(["max"], 3)] 1)

mmmByteCodeProg :: ByteCode.DPProg
mmmByteCodeProg = genSymSimple mmmByteCodeSpace "prog"

-- Component
mmmComponentSpace :: Component.DPProgSpace
mmmComponentSpace =
  Component.DPProgSpace
    [0, 0, 0]
    ( Component.ProgSpace
        4
        [ ("uminus", 1),
          ("add", 2),
          ("add", 2),
          ("add", 2),
          ("add", 2),
          ("max", 2),
          ("max", 2),
          ("max", 2)
        ]
        3
    )
    ( Component.ProgSpace
        3
        [ ("max", 3)
        ]
        1
    )

mmmComponentProg :: Component.DPProg
mmmComponentProg = genSymSimple mmmComponentSpace "prog"

main :: IO ()
main = mainFunc mmmProg mmmSpec elementGen mmmByteCodeProg mmmComponentProg