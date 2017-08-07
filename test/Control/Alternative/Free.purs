module Test.Control.Alternative.Free where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Alternative.Free (FreeAlternative, foldFreeAlternative, liftFreeAlternative)
import Control.Monad.Aff (Aff, makeAff, delay)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Data.Time.Duration (Milliseconds(..))
import Prelude (class Functor, Unit, pure, unit, ($), (*>), (>>=), (<>))
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

type Delay = Number

data ConsoleF next
  = ConsoleLog Delay String next
  | ConsoleError Delay String next
  | Exception String next

type Console a = FreeAlternative ConsoleF a

instance functorConsoleF ∷ Functor ConsoleF where
  map f (ConsoleLog delay string next) = ConsoleLog delay string (f next)
  map f (ConsoleError delay string next) = ConsoleError delay string (f next)
  map f (Exception string next) = Exception string (f next)

exception ∷ String → Console Unit
exception string = liftFreeAlternative (Exception string unit)

consoleLog ∷ Delay → String → Console Unit
consoleLog delay string = liftFreeAlternative (ConsoleLog delay string unit)

consoleError ∷ Delay → String → Console Unit
consoleError delay string = liftFreeAlternative (ConsoleError delay string unit)

affException ∷ ∀ e a. String → Aff e a
affException string = makeAff $ \err _ → err (error string)

delayLog ∷ ∀ e. Number → String → Aff (console ∷ CONSOLE | e) Unit
delayLog ms string = delay (Milliseconds ms) >>= \_ → log string

interpretConsole ∷ ∀ a e. ConsoleF a → Aff (console ∷ CONSOLE | e) a
interpretConsole (ConsoleLog delay string next) = (delayLog delay string) *> pure next
interpretConsole (ConsoleError delay string next) = (delayLog delay string) *> pure next
interpretConsole (Exception string next) = affException string *> pure next

foo :: String
foo = "Foo"

bar :: String
bar = "Bar"

program :: Console Unit
program = empty <|> exception "failed computation" <|> consoleLog 1100.0 foo <|> consoleLog 1100.0 bar

main ∷ Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Control.Alternative.Free" do
    it ("should print '" <> foo <> "' to the console") do
      foldFreeAlternative interpretConsole program
