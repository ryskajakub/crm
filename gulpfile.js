var gulp = require("gulp");
var shell = require("gulp-shell");
var concat = require("gulp-concat");
var clean = require('gulp-clean');

var faySources = 'src/**/*.hs'

gulp.task('compile', ['copy-resources'] , function() {
  return gulp.src('src/*.hs', {read: false})
    .pipe(shell([
      "fay --pretty <%= file.path %> --include src/ --output build/HaskellReact.js"
    ]))
});

gulp.task('watch', function() {
  gulp.watch(faySources, ['compile']);
  gulp.watch('files/*.js', ['copy-resources']);
});

gulp.task('copy-resources', function() {
  return gulp.src(['files/*.js', 'files/*.html'])
    .pipe(gulp.dest('build/'));
});

gulp.task('copy-test-resources', function() {
  return gulp.src(['test/*.js', 'files/*.js'])
    .pipe(gulp.dest('test_build/'));
});

gulp.task('test-compile', ['copy-test-resources'] , function () {
  return gulp.src('test/*.hs', {read: false})
    .pipe(shell([
      "fay --library --strict test/*.hs --pretty --include src/ --output test_build/HaskellReactSpec.js <%= file.path %> "
    ]))
});

var assemblyTestFile = function () {
  return gulp.src('test_build/*.js')
    .pipe(concat('all.js'))
    .pipe(gulp.dest('./test_single/'));
}

gulp.task('test-file', ['test-compile', 'copy-test-resources'], assemblyTestFile);

gulp.task('test-file-without-compile', ['copy-test-resources'], assemblyTestFile);

gulp.task('clean', function () {
  return gulp.src(['test_build/', 'build/', 'test_single/'], {read: false})
    .pipe(clean());
});

gulp.task('test-watch', function() {
  gulp.watch(['test/*.hs', faySources], ['test-file']);
  gulp.watch(['test/*.js', 'files/*.js'], ['test-file-without-compile']);
});
