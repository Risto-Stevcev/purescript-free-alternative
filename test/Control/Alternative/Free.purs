module Test.Control.Alternative.Free where

import Control.Alt ((<|>))
import Control.Alternative.Free (FreeAlternative, foldFreeAlternative, liftFreeAlternative)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff
import Effect.Class.Console (log)
import Effect.Exception (error, throwException)
import Prelude
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

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

delayLog ∷ Number → String → Aff Unit
delayLog ms string = delay (Milliseconds ms) >>= \_ → log string

interpretConsole ∷ ∀ a . ConsoleF a → ParAff a
interpretConsole (ConsoleLog delay string next) = parallel $ (delayLog delay string) *> pure next
interpretConsole (ConsoleError delay string next) = parallel $ (delayLog delay string) *> pure next
interpretConsole (Exception string next) = parallel $ throwError (error string) *> pure next

foo :: String
foo = "Foo"

bar :: String
bar = "Bar"

program :: Console Unit
program = empty <|> exception "failed computation" <|> consoleLog 1100.0 foo <|> consoleLog 1100.0 bar

main ∷ Effect Unit
main = launchAff_ $ run [consoleReporter] do
  describe "Control.Alternative.Free" do
    it ("should print '" <> foo <> "' to the console") do
      sequential $ foldFreeAlternative interpretConsole program
