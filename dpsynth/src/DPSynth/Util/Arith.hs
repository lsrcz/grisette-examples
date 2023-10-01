module DPSynth.Util.Arith (symMax) where

import Grisette (SOrd ((<~)), SimpleMergeable (mrgIte))

symMax :: (SimpleMergeable a, SOrd a) => a -> a -> a
symMax a b = mrgIte (a <~ b) b a
