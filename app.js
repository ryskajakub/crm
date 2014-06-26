requirejs.config({
  paths: {
    jquery: "bower_components/jquery/dist/jquery"
    , underscore: "bower_components/underscore/underscore"
    , backbone: "bower_components/backbone/backbone"
    , backbonelocalstorage: "bower_components/Backbone.localStorage/backbone.localStorage"
  }
});

requirejs(
  ["jquery", "underscore", "backbone", "backbonelocalstorage"],
  function ($, _, BB, LS) {
    console.log($);
    console.log(_);
    console.log(BB);
    console.log(LS);
  }
);
