exports.plural = function(word) {
  return require('pluralize')(word)
}

exports.snakeCase = function(word) {
  return require('snake-case').snakeCase(word)
}

exports.pascalCase = function(word) {
  return require('pascal-case').pascalCase(word)
}
