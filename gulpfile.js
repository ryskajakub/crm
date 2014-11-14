var gulp = require("gulp");
var shell = require("gulp-shell");
var mocha = require("gulp-mocha");

var faySources = 'src/*.hs'

gulp.task('fay-compile', function() {
  return gulp.src('src/Sample.hs', {read: false})
    .pipe(shell([
      "fay --pretty <%= file.path %> --package fay-text --output build/Sample.js"
    ]))
});

gulp.task('watch', function() {
  gulp.watch(faySources, ['fay-compile']);
});

gulp.task('copy-resources', function() {
  return gulp.src('files/*')
    .pipe(gulp.dest('build/'))
});

gulp.task('test', function() {
  return gulp.src('test/*.js', {read: false})
    .pipe(mocha());
});

gulp.task('default', ['copy-resources', 'watch']);
