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
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-test-resources', function() {
  return gulp.src('test/*.js')
    .pipe(gulp.dest('test_build/'));
});

gulp.task('test-compile', function () {
  return gulp.src('test/*.hs', {read: false})
    .pipe(shell([
      "fay --pretty <%= file.path %> --package fay-text --include src/ --output test_build/<%= mkJsFileName(file.path) %>"
    ], {
      templateData: {
        mkJsFileName: function (path) {
          var splittedPath = path.split('/');
          var fileName = splittedPath[splittedPath.length - 1];
          var splittedFileName = fileName.split('.');
          splittedFileName[splittedFileName.length - 1] = 'js';
          return splittedFileName.join('.');
        }
      }
    }))
});

gulp.task('execute-test', function() {
  return gulp.src('test/*.js', {read: false})
    .pipe(mocha());
});

gulp.task('test', ['copy-test-resources']);
gulp.task('default', ['copy-resources', 'watch']);
