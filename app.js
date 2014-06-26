requirejs.config({
  paths: {
    jquery: "bower_components/jquery/dist/jquery"
    , underscore: "bower_components/underscore/underscore"
    , backbone: "bower_components/backbone/backbone"
    , backbonelocalstorage: "bower_components/Backbone.localStorage/backbone.localStorage"
  }
});

requirejs(
  ["scripts/main"],
  function (main) {
    main.run();
  }
);
