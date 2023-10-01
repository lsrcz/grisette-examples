{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DPSynth.Experimental.ByteCodeCached where

import qualified DPSynth.ByteCode as ByteCode
import qualified DPSynth.Concrete as Concrete
import DPSynth.Experimental.CEGIS
  ( CEGISAlgorithm
      ( initialSynthConditionState,
        initialVerifierState,
        runVerifier,
        synthCondition
      ),
    VerificationResult (VerificationFoundCex, VerificationSuccess),
  )
import DPSynth.Experimental.ConcreteCached
  ( InputGen (InputGen),
    quickCheckDPProgMaximizeCaching,
  )
import Grisette (SEq ((==~)), ToSym (toSym))
import Test.QuickCheck (Gen)

data SynthByteCodeCached = SynthByteCodeCached
  { synthByteCodeCachedMaxInputListLength :: Int,
    synthByteCodeCachedFuzzingSpec :: [Concrete.Val] -> [Concrete.Val],
    synthByteCodeCachedFuzzingGenerator :: Gen Concrete.Val
  }

instance
  CEGISAlgorithm
    SynthByteCodeCached
    Concrete.DPProgCex
    ()
    [InputGen]
    ByteCode.DPProg
    Concrete.DPProg
  where
  synthCondition _ _ prog cex _ =
    let result = ByteCode.runDPProg prog (toSym (Concrete.dpProgCexInput cex))
     in ( toSym
            ( return (Concrete.dpProgCexOutput cex) ::
                ByteCode.Context [Concrete.Val]
            )
            ==~ (result :: ByteCode.Context [ByteCode.Val]),
          ()
        )
  runVerifier problem gens prog = do
    r <-
      quickCheckDPProgMaximizeCaching
        (synthByteCodeCachedFuzzingSpec problem)
        (synthByteCodeCachedMaxInputListLength problem)
        gens
        prog
    case r of
      Nothing -> return (VerificationSuccess, [])
      Just (cex, gens') -> return (VerificationFoundCex cex, gens')
  initialSynthConditionState _ = ()
  initialVerifierState problem =
    [ InputGen i 10000 [] (synthByteCodeCachedFuzzingGenerator problem)
      | i <- [1 .. synthByteCodeCachedMaxInputListLength problem]
    ]
