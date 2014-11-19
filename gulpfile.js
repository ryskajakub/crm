var gulp = require("gulp");
var shell = require("gulp-shell");
var concat = require("gulp-concat");
var clean = require('gulp-clean');
var webpack = require('gulp-webpack');

var faySources = 'src/**/*.hs'

// main

gulp.task('copy-resources', ['copy-bootstrap'], function() {
  return gulp.src(['files/*.html'])
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-bootstrap', function () {
  return gulp.src(['bootstrap/**/*'], {base: 'bootstrap'})
    .pipe(gulp.dest('build/bootstrap'));
});

gulp.task('compile', function() {
  return gulp.src('src/Main.hs', {read: false})
    .pipe(shell([
      "fay --Wall --pretty <%= file.path %> --include src/ --output tmp/HaskellReact.js"
    ]));
});

gulp.task('webpack', ['compile', 'copy-resources'], function () {
  return gulp.src('tmp/HaskellReact.js', {read: false})
    .pipe(webpack({
      entry: "./tmp/HaskellReact.js"
      , output: {
        filename: "haskell-react-packed.js"
      }
    }))
    .pipe(gulp.dest('build/'));
});

gulp.task('watch', function() {
  gulp.watch([faySources, 'files/*.html', 'files/*.js'], ['webpack']);
});

// test

gulp.task('test-compile', function () {
  return gulp.src('test/*.hs', {read: false})
    .pipe(shell([
      "fay --Wall --strict test/*.hs --pretty --include src/ --output tmp/HaskellReactSpec.js <%= file.path %> "
    ]))
});

var testWebpack = function () {
  return gulp.src('test/haskellReactSpec.js', {read: false})
    .pipe(webpack({
      entry: "./test/haskellReactSpec.js"
      , output: {
        filename: "haskell-react-spec-packed.js"
      }
    }))
    .pipe(gulp.dest('test_build/'));
};

gulp.task('test-file', ['test-compile'], testWebpack);

gulp.task('test-file-without-compile', testWebpack);

gulp.task('test-watch', function() {
  gulp.watch(['test/*.hs', faySources], ['test-file']);
  gulp.watch(['test/*.js', 'files/*.js'], ['test-file-without-compile']);
});

// other

gulp.task('clean', function () {
  return gulp.src(['test_build/', 'build/'], {read: false})
    .pipe(clean());
});
