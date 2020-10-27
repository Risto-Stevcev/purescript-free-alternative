module Control.Alternative.Free where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Alternative (class Alternative)
import Control.Applicative.Free (FreeAp, foldFreeAp, hoistFreeAp, liftFreeAp)
import Data.Foldable (foldr)
import Data.Functor.Compose (Compose(..))
import Data.List (List, singleton)
import Prelude

type FreeAlternative f a = Compose List (FreeAp f) a

liftFreeAlternative ∷ ∀ f a. f a → FreeAlternative f a
liftFreeAlternative = Compose <<< singleton <<< liftFreeAp

lowerFreeAlternative ∷ ∀ f a. Alternative f => FreeAlternative f a → f a
lowerFreeAlternative a = foldFreeAlternative identity a

foldFreeAlternative ∷ ∀ f g a. Alternative g => (f ~> g) → FreeAlternative f a → g a
foldFreeAlternative f (Compose xs) = foldr (\elem acc → foldFreeAp f elem <|> acc) empty xs

hoistFreeAlternative ∷ ∀ f g a. (f ~> g) → FreeAlternative f a → FreeAlternative g a
hoistFreeAlternative f (Compose a) = Compose $ map (hoistFreeAp f) a
