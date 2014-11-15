var gulp = require("gulp");
var shell = require("gulp-shell");
var concat = require("gulp-concat");
var clean = require('gulp-clean');

var faySources = 'src/*.hs'

gulp.task('compile', function() {
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
  return gulp.src(['test/*.js', 'files/*.js'])
    .pipe(gulp.dest('test_build/'));
});

gulp.task('test-compile', ['copy-test-resources'] , function () {
  return gulp.src('test/Class.hs', {read: false})
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

gulp.task('test-file', ['test-compile', 'copy-test-resources'], function () {
  return gulp.src('test_build/*.js')
    .pipe(concat('all.js'))
    .pipe(gulp.dest('./test_single/'));
});

gulp.task('clean', function () {
  return gulp.src(['test_build/', 'build/', 'test_single/'], {read: false})
    .pipe(clean());
});

gulp.task('test-watch', function() {
  gulp.watch('test/*.js', ['test-file']);
});

gulp.task('default', ['copy-resources', 'watch']);
