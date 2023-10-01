{-# LANGUAGE DeriveDataTypeable #-}

module DPSynth.Util.MainFunc (mainFunc) where

import qualified DPSynth.ByteCode as ByteCode
import qualified DPSynth.Component as Component
import DPSynth.Concrete (DPProgCex (dpProgCexInput))
import qualified DPSynth.Concrete as Concrete
import qualified DPSynth.Experimental.ByteCodeCached as ByteCodeCached
import DPSynth.Experimental.CEGIS (CEGISResult (CEGISSuccess), cegis)
import qualified DPSynth.Experimental.ComponentCached as ComponentCached
import DPSynth.Util.TimeIt (timeItAll)
import Data.Data (Data, Typeable)
import Grisette
  ( SMTConfig,
    boolector,
    precise,
    z3,
  )
import System.Console.CmdArgs (cmdArgs)
import Test.QuickCheck (Gen, vectorOf)

data Args = Args {algorithm :: String, solver :: String}
  deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args "check" "z3"

mainFuncBase :: (SMTConfig -> String -> IO ()) -> IO ()
mainFuncBase f = do
  args <- cmdArgs defaultArgs
  config <- case solver args of
    "z3" -> return z3
    "boolector" -> return boolector
    _ -> error $ "Unknown solver: " ++ solver args
  f config (algorithm args)

mainFunc ::
  Concrete.DPProg ->
  ([Concrete.Val] -> [Concrete.Val]) ->
  Gen Concrete.Val ->
  ByteCode.DPProg ->
  Component.DPProg ->
  IO ()
mainFunc conProg conSpec elementGen bcProg compProg =
  mainFuncBase $ \config algo -> do
    let qcProg prog = do
          r <-
            Concrete.quickCheckDPProg
              conSpec
              1000
              [vectorOf i elementGen | i <- [3, 4, 5]]
              prog
          case r of
            Nothing -> putStrLn "No counterexample found"
            Just (cex, _) -> do
              putStr "Counterexample found: "
              print cex
              putStr "Program output: "
              print $ Concrete.runDPProg prog (dpProgCexInput cex)
    let checkCEGISResult res =
          case res of
            CEGISSuccess prog' -> do
              putStrLn "------- Checking concrete program -------"
              qcProg prog'
            _ -> return ()
    let grisetteConfig = precise config
    case algo of
      "bytecode" -> do
        putStrLn "------- Synthesis (bytecode) -------"
        prog <-
          timeItAll "Synthesis (bytecode)" $
            cegis
              grisetteConfig
              (ByteCode.SynthByteCode 4 1000 conSpec elementGen)
              bcProg
        print prog
        checkCEGISResult prog
      "bytecode-cached" -> do
        putStrLn "------- Synthesis (bytecode-cached) -------"
        prog <-
          timeItAll "Synthesis (bytecode-cached)" $
            cegis
              grisetteConfig
              (ByteCodeCached.SynthByteCodeCached 4 conSpec elementGen)
              bcProg
        print prog
        checkCEGISResult prog
      "component" -> do
        putStrLn "------- Synthesis (component) -------"
        prog <-
          timeItAll "Synthesis (component)" $
            cegis
              grisetteConfig
              (Component.SynthComponent 4 1000 conSpec elementGen)
              compProg
        print prog
        checkCEGISResult prog
      "component-cached" -> do
        putStrLn "------- Synthesis (component-cached) -------"
        prog <-
          timeItAll "Synthesis (component-cached)" $
            cegis
              grisetteConfig
              (ComponentCached.SynthComponentCached 4 conSpec elementGen)
              compProg
        print prog
        checkCEGISResult prog
      _ -> do
        putStrLn "------- Checking concrete program -------"
        qcProg conProg
