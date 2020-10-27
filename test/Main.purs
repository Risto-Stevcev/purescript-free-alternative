module Test.Main where

import Prelude
import Effect
import Test.Control.Alternative.Free (main) as Free

main :: Effect Unit
main = Free.main
