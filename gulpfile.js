var gulp = require("gulp");
var shell = require("gulp-shell");
var concat = require("gulp-concat");
var clean = require('gulp-clean');
var webpack = require('gulp-webpack');

var faySources = 'src/**/*.hs'

// main

gulp.task('copy-resources', function() {
  return gulp.src(['files/*.html'])
    .pipe(gulp.dest('build/'));
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
})

gulp.task('watch', function() {
  gulp.watch([faySources, 'files/*.html', 'files/*.js'], ['webpack']);
});

// test

gulp.task('copy-test-resources', function() {
  return gulp.src(['test/*.js', 'files/*.js'])
    .pipe(gulp.dest('test_build/'));
});

gulp.task('test-compile', ['copy-test-resources'] , function () {
  return gulp.src('test/*.hs', {read: false})
    .pipe(shell([
      "fay --Wall --library --strict test/*.hs --pretty --include src/ --output test_build/HaskellReactSpec.js <%= file.path %> "
    ]))
});

var assemblyTestFile = function () {
  return gulp.src('test_build/*.js')
    .pipe(concat('all.js'))
    .pipe(gulp.dest('./test_single/'));
}

gulp.task('test-file', ['test-compile', 'copy-test-resources'], assemblyTestFile);

gulp.task('test-file-without-compile', ['copy-test-resources'], assemblyTestFile);

gulp.task('test-watch', function() {
  gulp.watch(['test/*.hs', faySources], ['test-file']);
  gulp.watch(['test/*.js', 'files/*.js'], ['test-file-without-compile']);
});

// other

gulp.task('clean', function () {
  return gulp.src(['test_build/', 'build/', 'test_single/'], {read: false})
    .pipe(clean());
});
