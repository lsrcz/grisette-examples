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
mssProg :: Concrete.DPProg
mssProg =
  Concrete.DPProg
    [0, 0]
    ( Concrete.Prog
        3
        [ Concrete.Stmt "zero" [] 3,
          Concrete.Stmt "max" [1, 3] 4,
          Concrete.Stmt "add" [0, 4] 5,
          Concrete.Stmt "max" [1, 2] 6
        ]
        [5, 6]
    )
    ( Concrete.Prog
        2
        [Concrete.Stmt "max" [0, 1] 2]
        [2]
    )

mssSpec :: [Concrete.Val] -> [Concrete.Val]
mssSpec l = [mssSpec0 (take n l) | n <- [1 .. length l]]

mssSpec0 :: [Concrete.Val] -> Concrete.Val
mssSpec0 l = maximum $ fmap (sum . zipWith (*) l) allLists
  where
    allLists = filter listOk0 $ replicateM (length l) [0, 1]
    listOk0 [] = True
    listOk0 (0 : xs) = listOk0 xs
    listOk0 (1 : xs) = listOk1 xs
    listOk0 _ = False
    listOk1 [] = True
    listOk1 (1 : xs) = listOk1 xs
    listOk1 (0 : xs) = all (== 0) xs
    listOk1 _ = False

elementGen :: Gen Concrete.Val
elementGen = chooseEnum (-16, 16)

-- ByteCode
mssByteCodeSpace :: ByteCode.DPProgSpace
mssByteCodeSpace =
  ByteCode.DPProgSpace
    [0, 0]
    ( ByteCode.ProgSpace
        3
        [ (["zero"], 0),
          (["add", "max"], 2),
          (["add", "max"], 2),
          (["add", "max"], 2)
        ]
        2
    )
    (ByteCode.ProgSpace 2 [(["max"], 2)] 1)

mssByteCodeProg :: ByteCode.DPProg
mssByteCodeProg = genSymSimple mssByteCodeSpace "prog"

-- Component
mssComponentSpace :: Component.DPProgSpace
mssComponentSpace =
  Component.DPProgSpace
    [0, 0]
    ( Component.ProgSpace
        3
        [("zero", 0), ("add", 2), ("max", 2), ("max", 2)]
        2
    )
    ( Component.ProgSpace
        2
        [("max", 2)]
        1
    )

mssComponentProg :: Component.DPProg
mssComponentProg = genSymSimple mssComponentSpace "prog"

main :: IO ()
main = mainFunc mssProg mssSpec elementGen mssByteCodeProg mssComponentProg
