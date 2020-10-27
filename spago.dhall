{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "free-alternative"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "spec"
  , "free"
  , "freeap"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
