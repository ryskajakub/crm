module.exports = function(grunt) {

  grunt.initConfig({
    compass: {
      dist: {
        options: {
          sassDir: "sass"
        }
      }
    } ,
  });

  grunt.loadNpmTasks('grunt-contrib-compass');

};
