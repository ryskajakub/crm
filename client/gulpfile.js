var gulp = require("gulp");
var shell = require("gulp-shell");
var clean = require('gulp-clean');
var webpack = require('gulp-webpack');
var _ = require('underscore');

var commonSources = [
  '../../fay-react/core/src/' ,
  '../../fay-react/wrappers/src/' ,
  './shared/' ,
  '../../fay-jquery/src/' ,
  '../../fay-moment/src/' ];
var playgroundSources = commonSources.concat('playground/');
var mainSources = commonSources.concat('src/');

var mkSourcesAsGlob = function(sources) {
  return _.map(sources, function(source) {
    return source + '**/*.hs';
  });
}

var mkSourcesCommaDelimited = function(sources) {
  return "'" + _.reduce(_.tail(sources), function(acc, elem) {
    return acc + ',' + elem;
  }, _.head(sources)) + "'";
}

var sourcesAsGlob = mkSourcesAsGlob(mainSources);
var sourcesCommaDelimited = mkSourcesCommaDelimited(mainSources);

var playgroundSourcesAsGlob = mkSourcesAsGlob(playgroundSources);
var playgroundSourcesCommaDelimited = mkSourcesCommaDelimited(playgroundSources);

// main

gulp.task('copy-resources', ['copy-bootstrap', 'copy-jquery'], function() {
  return gulp.src(['files/*'])
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-bootstrap', function () {
  return gulp.src(['bootstrap/**/*'], {base: 'bootstrap'})
    .pipe(gulp.dest('build/bootstrap'));
});

gulp.task('copy-jasny', function () {
  return gulp.src(['bower_components/jasny-bootstrap/dist/**'])
    .pipe(gulp.dest('build/jasny'));
});

gulp.task('copy-jquery', function () {
  return gulp.src(['bower_components/jquery/dist/jquery.js'])
    .pipe(gulp.dest('build/'));
});

gulp.task('compile', function() {
  var fayCommand = "../server/.cabal-sandbox/bin/fay --strict HaskellReact.ReactCalendar --Wall --pretty <%= file.path %> --include " + sourcesCommaDelimited + " --output tmp/HaskellReact.js --package fay-dom";
  return gulp.src('src/Main.hs', {read: false})
    .pipe(shell([fayCommand]));
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
  gulp.watch(['files/*'], ['copy-resources']);
  gulp.watch(sourcesAsGlob, ['webpack']);
});

gulp.task('default', ['watch']);

// playground

gulp.task('playground-compile', function() {
  var fayCommand = "fay --Wall --pretty --include " + playgroundSourcesCommaDelimited + " --output tmp/Playground.js --package fay-dom <%= file.path %>";
  return gulp.src('playground/Main.hs', {read: false})
    .pipe(shell([fayCommand]));
});

gulp.task('playground-webpack', ['playground-compile', 'copy-resources'], function () {
  return gulp.src('tmp/Playground.js', {read: false})
    .pipe(webpack({
      entry: "./tmp/Playground.js"
      , output: {
        filename: "haskell-react-packed.js"
      }
    }))
    .pipe(gulp.dest('build/'));
});

gulp.task('playground', function () {
  gulp.watch(playgroundSourcesAsGlob, ['playground-webpack']);
});

// other

gulp.task('clean', function () {
  return gulp.src(['build/'], {read: false})
    .pipe(clean());
});
