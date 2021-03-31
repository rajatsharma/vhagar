exports.replace = function(options) {
  return function () {
    require('replace-in-file').sync(options)
  }
}
