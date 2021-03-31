let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210324/packages.dhall sha256:b4564d575da6aed1c042ca7936da97c8b7a29473b63f4515f09bb95fae8dddab

in upstream

with handlebars = {
  dependencies = [] : List Text,
  repo = "https://github.com/rajatsharma/purescript-handlebars",
  version = "4db1b2f17e30a1ffed2710ca1cdb86d5e001d113"
}

with node-args = {
  dependencies = [ "lists" ],
  repo = "https://github.com/rajatsharma/purescript-node-args.git",
  version = "9cbe6b96595134b5971928975a63f3d2d746aae0"
}
