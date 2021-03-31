{ name = "kensai"
, dependencies =
  [ "console"
  , "effect"
  , "handlebars"
  , "node-args"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "admin/**/*.purs" ]
}
