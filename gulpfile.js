var gulp = require("gulp");
var shell = require("gulp-shell");

var faySources = 'src/*.hs'

gulp.task('fay-compile', function() {
  return gulp.src('src/Sample.hs', {read: false})
    .pipe(shell([
      "fay --pretty <%= file.path %> --package fay-text --output Sample.js"
    ]))
});

gulp.task('watch', function() {
  gulp.watch(faySources, ['fay-compile']);
});

gulp.task('default', ['watch']);
