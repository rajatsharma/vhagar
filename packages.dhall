let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210324/packages.dhall sha256:b4564d575da6aed1c042ca7936da97c8b7a29473b63f4515f09bb95fae8dddab

in upstream with common-utils = {
  dependencies = [ "effect", "node-fs", "prelude", "node-process" ],
  repo = "https://github.com/rajatsharma/purescript-common-utils.git",
  version = "76f810ef8e7132e18fbaa2650d9391c6e6db4d9c"
}
