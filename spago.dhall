{ name = "kensai"
, dependencies =
  [ "common-utils"
  , "console"
  , "effect"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "admin/**/*.purs" ]
}
