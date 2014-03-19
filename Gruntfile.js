module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 
    
        clean: ["externs", "js"],
    
        "purescript-make": {
            options: {
                tco: true,
                magicDo: true
            },
            all: {
                options: { },
                files: {
                    _: [ "src/**/*.purs.hs"
                       , "bower_components/purescript-*/src/**/*.purs"
                       , "bower_components/purescript-*/src/**/*.purs.hs"
                       ]
                }
            }
        }
        
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["purescript-make:all"]);
};
