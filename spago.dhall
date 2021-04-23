{ name = "kensai"
, dependencies =
  [ "common-utils"
  , "console"
  , "effect"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "psci-support"
  , "lists"
  , "node-buffer"
  , "prelude"
  , "node-streams"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "admin/**/*.purs" ]
}
