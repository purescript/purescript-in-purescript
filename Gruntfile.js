module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs*"
    ],
    
    watch: {
      lib: {
        files: ["<%=libFiles%>"],
        tasks: ["pscMake"]
      }
    },
    
    clean: ["output"],
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"]

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks('grunt-contrib-watch');
  
  grunt.registerTask("make", ["pscMake", "dotPsci"]);
  grunt.registerTask("default", ["make"]);
};
