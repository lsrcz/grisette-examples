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
masProg :: Concrete.DPProg
masProg =
  Concrete.DPProg
    [0, 0, 0]
    ( Concrete.Prog
        4
        [ Concrete.Stmt "uminus" [0] 4,
          Concrete.Stmt "add" [0, 2] 5,
          Concrete.Stmt "add" [1, 4] 6,
          Concrete.Stmt "max" [0, 5] 7,
          Concrete.Stmt "max" [4, 6] 8,
          Concrete.Stmt "max" [1, 2, 3] 9
        ]
        [7, 8, 9]
    )
    (Concrete.Prog 3 [Concrete.Stmt "max" [0, 1, 2] 3] [3])

masSpec :: [Concrete.Val] -> [Concrete.Val]
masSpec l = [masSpec0 (take n l) | n <- [1 .. length l]]

masSpec0 :: [Concrete.Val] -> Concrete.Val
masSpec0 l = maximum $ fmap (sum . zipWith (*) l) allLists
  where
    allLists = filter listOk $ replicateM (length l) [-1, 0, 1]
    listOk [] = True
    listOk (0 : xs) = listOk xs
    listOk (1 : xs) = listOkm1 xs
    listOk (-1 : xs) = listOk1 xs
    listOk _ = False
    listOkm1 [] = True
    listOkm1 (-1 : xs) = listOk1 xs
    listOkm1 (0 : xs) = all (== 0) xs
    listOkm1 _ = False
    listOk1 [] = True
    listOk1 (1 : xs) = listOkm1 xs
    listOk1 (0 : xs) = all (== 0) xs
    listOk1 _ = False

elementGen :: Gen Concrete.Val
elementGen = chooseEnum (-16, 16)

-- ByteCode
masByteCodeSpace :: ByteCode.DPProgSpace
masByteCodeSpace =
  ByteCode.DPProgSpace
    [0, 0, 0]
    ( ByteCode.ProgSpace
        4
        [ (["uminus"], 1),
          (["add", "max"], 2),
          (["add", "max"], 2),
          (["add", "max"], 2),
          (["add", "max"], 2),
          (["max"], 3)
        ]
        3
    )
    (ByteCode.ProgSpace 3 [] 1)

masByteCodeProg :: ByteCode.DPProg
masByteCodeProg = genSymSimple masByteCodeSpace "prog"

-- Component
masComponentSpace :: Component.DPProgSpace
masComponentSpace =
  Component.DPProgSpace
    [0, 0, 0]
    ( Component.ProgSpace
        4
        [ ("uminus", 1),
          ("add", 2),
          ("add", 2),
          ("max", 2),
          ("max", 2),
          ("max", 3)
        ]
        3
    )
    (Component.ProgSpace 3 [] 1)

masComponentProg :: Component.DPProg
masComponentProg = genSymSimple masComponentSpace "prog"

main :: IO ()
main = mainFunc masProg masSpec elementGen masByteCodeProg masComponentProg
