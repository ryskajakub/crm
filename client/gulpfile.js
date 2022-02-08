var gulp = require("gulp");
var shell = require("gulp-shell");
var clean = require('gulp-clean');
var webpack = require('gulp-webpack');
const sass = require('gulp-sass')(require('sass'));
var _ = require('underscore');

var commonSources = [
  '../fay-react/core/src/' ,
  '../fay-react/wrappers/src/' ,
  './shared/' ,
  '../fay-dom/src/' ,
  '../fay-jquery/src/' ,
  '../fay-moment/src/' ,
  '../fay-googlemaps/src' ];
var playgroundSources = commonSources.concat('playground/');
var mainSources = commonSources.concat('src/').concat('generated-api/src');

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
gulp.task('compile-sass', function () {
  gulp.src('./scss/*.scss')
    .pipe(sass({errLogToConsole: true}))
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-resources', ['copy-bootstrap', 'copy-jquery', 'copy-images', 'copy-jasny'], function() {
  return gulp.src(['files/*'])
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-bootstrap', function () {
  return gulp.src(['bootstrap/**/*'], {base: 'bootstrap'})
    .pipe(gulp.dest('build/bootstrap'));
});

gulp.task('copy-images', function () {
    return gulp.src(['images/*'], {base: 'images'})
        .pipe(gulp.dest('build/images'));
});

gulp.task('copy-jasny', function () {
  return gulp.src(['node_modules/jasny-bootstrap/dist/**'])
    .pipe(gulp.dest('build/jasny'));
});

gulp.task('copy-jquery', function () {
  return gulp.src(['node_modules/jquery/dist/jquery.min.js'])
    .pipe(gulp.dest('build/'));
});

gulp.task('compile', function() {
  var fayCommand = "fay --no-optimized-newtypes --strict HaskellReact.ReactCalendar --pretty <%= file.path %> --include " + sourcesCommaDelimited + " --output tmp/HaskellReact.js";
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
  gulp.watch(['scss/*'], ['compile-sass']);
  gulp.watch(sourcesAsGlob, ['webpack']);
});

gulp.task('default', ['watch']);

// playground

gulp.task('playground-compile', function() {
  var fayCommand = "/Users/ryskaj/.cabal/bin/fay --Wall --pretty --include " + playgroundSourcesCommaDelimited + " --output tmp/Playground.js <%= file.path %>";
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
