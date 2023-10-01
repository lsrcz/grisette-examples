module DPSynth.Util.TimeIt where

import System.Clock
  ( Clock (Monotonic, ProcessCPUTime),
    TimeSpec (TimeSpec),
    getTime,
  )
import Text.Printf (printf)

diff :: TimeSpec -> TimeSpec -> Double
diff (TimeSpec s1 n1) (TimeSpec s2 n2) =
  (a2 - a1) / (fromIntegral (10 ^ (9 :: Integer) :: Integer) :: Double)
  where
    a1 = (fromIntegral s1 * 10 ^ (9 :: Integer)) + fromIntegral n1
    a2 = (fromIntegral s2 * 10 ^ (9 :: Integer)) + fromIntegral n2

timeItAll :: String -> IO a -> IO a
timeItAll str x = do
  startMono <- getTime Monotonic
  startProcessCPU <- getTime ProcessCPUTime
  r <- x
  endMono <- getTime Monotonic
  endProcessCPU <- getTime ProcessCPUTime
  printf "%s -- Mono clock: %.6fs\n" str (diff startMono endMono)
  printf "%s -- CPU clock:  %.6fs\n" str (diff startProcessCPU endProcessCPU)
  return r
