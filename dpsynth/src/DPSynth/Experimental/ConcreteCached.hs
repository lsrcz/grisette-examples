{-# LANGUAGE ExplicitNamespaces #-}

module DPSynth.Experimental.ConcreteCached
  ( InputGen (..),
    quickCheckDPProgMaximizeCaching,
  )
where

import DPSynth.Concrete (DPProg, DPProgCex (DPProgCex), Val, runDPProg)
import Data.List (sortOn)
import Test.QuickCheck.Counterexamples
  ( Args (chatty, maxSuccess),
    Gen,
    forAll,
    quickCheckWith,
    stdArgs,
    vectorOf,
    type (:&:) ((:&:)),
  )

data InputGen = InputGen
  { len :: Int,
    num :: Int,
    curr :: [Val],
    elementGen :: Gen Val
  }

inputGen :: InputGen -> Gen [Val]
inputGen igen =
  (curr igen ++)
    <$> vectorOf
      (len igen - length (curr igen))
      (elementGen igen)

quickCheckDPProgMaximizeCaching ::
  ([Val] -> [Val]) ->
  Int ->
  [InputGen] ->
  DPProg ->
  IO (Maybe (DPProgCex, [InputGen]))
quickCheckDPProgMaximizeCaching _ _ [] _ = return Nothing
quickCheckDPProgMaximizeCaching spec maxGen (gen : gens) prog = do
  r <- quickCheckWith stdArgs {chatty = False, maxSuccess = num gen} $
    forAll (inputGen gen) $
      \input -> runDPProg prog input == Right (spec input)
  case r of
    Nothing -> quickCheckDPProgMaximizeCaching spec maxGen gens prog
    Just (cex :&: ()) ->
      return $
        Just
          ( DPProgCex cex (spec cex),
            sortOn len $
              filter (\v -> maxGen >= len v) $
                [ InputGen l 100 cex (elementGen gen)
                  | l <- [length cex + 1 .. maxGen]
                ]
                  ++ gen
                  : gens
          )
