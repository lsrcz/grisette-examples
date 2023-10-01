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
misProg :: Concrete.DPProg
misProg =
  Concrete.DPProg
    [0, 0]
    ( Concrete.Prog
        3
        [ Concrete.Stmt "add" [0, 2] 3,
          Concrete.Stmt "max" [1, 2] 4
        ]
        [3, 4]
    )
    ( Concrete.Prog
        2
        [ Concrete.Stmt "max" [0, 1] 2
        ]
        [2]
    )

misSpec :: [Concrete.Val] -> [Concrete.Val]
misSpec l = [misSpec0 (take n l) | n <- [1 .. length l]]

misSpec0 :: [Concrete.Val] -> Concrete.Val
misSpec0 l = maximum $ fmap (sum . zipWith (*) l) allLists
  where
    allLists = filter isNotConsecutive $ replicateM (length l) [0, 1]
    isNotConsecutive [] = True
    isNotConsecutive [_] = True
    isNotConsecutive (1 : 1 : _) = False
    isNotConsecutive (_ : x : xs) = isNotConsecutive $ x : xs

elementGen :: Gen Concrete.Val
elementGen = chooseEnum (-16, 16)

-- ByteCode
misByteCodeSpace :: ByteCode.DPProgSpace
misByteCodeSpace =
  ByteCode.DPProgSpace
    [0, 0]
    ( ByteCode.ProgSpace
        3
        [ (["add", "max"], 2),
          (["add", "max"], 2)
        ]
        2
    )
    ( ByteCode.ProgSpace
        2
        [ (["max"], 1)
        ]
        1
    )

misByteCodeProg :: ByteCode.DPProg
misByteCodeProg = genSymSimple misByteCodeSpace "prog"

-- Component
misComponentSpace :: Component.DPProgSpace
misComponentSpace =
  Component.DPProgSpace
    [0, 0]
    (Component.ProgSpace 3 [("add", 2), ("max", 2)] 2)
    (Component.ProgSpace 2 [("max", 1)] 1)

misComponentProg :: Component.DPProg
misComponentProg = genSymSimple misComponentSpace "prog"

main :: IO ()
main = mainFunc misProg misSpec elementGen misByteCodeProg misComponentProg
