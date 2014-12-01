var gulp = require("gulp");
var shell = require("gulp-shell");
var clean = require('gulp-clean');
var webpack = require('gulp-webpack');
var _ = require('underscore');

var sources = [
  'src/'
  , '../../haskell-react/core/src/'
  , '../../haskell-react/wrappers/src/'
  , './shared/'
];

var sourcesAsGlob = _.map(sources, function(source) {
  return source + '**/*.hs';
});
var sourcesCommaDelimited = "'" + _.reduce(_.tail(sources), function(acc, elem) {
  return acc + ',' + elem;
}, _.head(sources)) + "'";

// main

gulp.task('copy-resources', ['copy-bootstrap'], function() {
  return gulp.src(['files/*.html', 'files/*.css'])
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-bootstrap', function () {
  return gulp.src(['bootstrap/**/*'], {base: 'bootstrap'})
    .pipe(gulp.dest('build/bootstrap'));
});

gulp.task('generate-rest-client', function () {
  // just read anything basically
  return gulp.src('gulpfile.js', {read: false})
    .pipe(shell([
      '../server/.cabal-sandbox/bin/crm-gen-client -j > tmp/CrmApi.js'
    ]))
});


gulp.task('compile', function() {
  var fayCommand = "fay --Wall --pretty <%= file.path %> --include " + sourcesCommaDelimited + " --output tmp/HaskellReact.js --package 'fay-dom'";
  return gulp.src('src/Main.hs', {read: false})
    .pipe(shell([fayCommand]));
});

gulp.task('webpack', ['compile', 'copy-resources', 'generate-rest-client'], function () {
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
  var watchedPaths = _.union(['files/*'], sourcesAsGlob);
  gulp.watch(watchedPaths, ['webpack']);
});

gulp.task('default', ['watch']);

// other

gulp.task('clean', function () {
  return gulp.src(['build/'], {read: false})
    .pipe(clean());
});
