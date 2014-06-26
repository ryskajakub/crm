module.exports = function(grunt) {

  grunt.initConfig({
    compass: {
      dist: {
        options: {
          sassDir: "sass"
          , require: "singularitygs"
        }
      }
    } ,
  });

  grunt.loadNpmTasks('grunt-contrib-compass');

};
