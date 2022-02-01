var gulp = require("gulp");
var shell = require("gulp-shell");
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
      "fay --package 'haskell-react' --Wall --pretty <%= file.path %> --include src/ --output tmp/HaskellReact.js"
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

// other

gulp.task('clean', function () {
  return gulp.src(['build/'], {read: false})
    .pipe(clean());
});
