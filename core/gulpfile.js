var gulp = require("gulp");
var shell = require("gulp-shell");
var clean = require('gulp-clean');
var webpack = require('gulp-webpack');

var faySources = 'src/**/*.hs';

gulp.task('test-compile', function () {
  return gulp.src('test/*.hs', {read: false})
    .pipe(shell([
      "fay --Wall --strict test/*.hs --pretty --include src/ --output tmp/HaskellReactSpec.js <%= file.path %> "
    ]));
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
  gulp.watch(['test/*.hs', faySources, ''], ['test-file']);
  gulp.watch(['test/*.js', 'files/*.js'], ['test-file-without-compile']);
});

// other

gulp.task('clean', function () {
  return gulp.src(['test_build/'], {read: false})
    .pipe(clean());
});
