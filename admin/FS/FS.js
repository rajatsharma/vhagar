exports.replace = function(options) {
  return function () {
    require('replace-in-file').sync(options)
  }
}

exports.copyDir = function(from) {
  return function (to) {
    return function () {
      require('fs-extra').copySync(from, to)
    }
  }
}
