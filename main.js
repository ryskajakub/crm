require.config({

  paths: {
    "react": "bower_components/jsx-requirejs-plugin/js/react-with-addons-0.10.0"
    , "JSXTransformer": "bower_components/jsx-requirejs-plugin/js/JSXTransformer-0.10.0"
    , "text": "bower_components/jsx-requirejs-plugin/js/text"
    , "jsx": "bower_components/jsx-requirejs-plugin/js/jsx"
    , "underscore": "bower_components/underscore/underscore"
    , "backbone": "bower_components/backbone/backbone"
    , "jquery": "bower_components/jquery/dist/jquery"
  },

  jsx: {
    fileExtension: ".jsx"
  }
});

require(["jsx!app"], function(React, App) {
  
});
