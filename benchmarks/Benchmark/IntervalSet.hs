module Benchmark.IntervalSet where
import Control.DeepSeq
import Data.IntervalSet

instance NFData Tree where
        rnf (Node l i r) = seq (rnf l) (seq i (rnf r))
        rnf t = seq t ()

instance NFData IntervalSet where
        rnf (IntervalSet i t) = seq i (rnf t)
        rnf s = seq s ()