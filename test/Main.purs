module Test.Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Test.Control.Alternative.Free (main) as Free
import Test.Spec.Runner (RunnerEffects)

main :: Eff (RunnerEffects ()) Unit
main = Free.main
