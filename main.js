require.config({

  paths: {
    "react": "bower_components/jsx-requirejs-plugin/js/react-with-addons-0.10.0"
    , "JSXTransformer": "bower_components/jsx-requirejs-plugin/js/JSXTransformer-0.10.0"
    , "text": "bower_components/jsx-requirejs-plugin/js/text"
    , "jsx": "bower_components/jsx-requirejs-plugin/js/jsx"
  },

  jsx: {
    fileExtension: '.jsx'
  }
});

require(['react', 'jsx!app'], function(React, App) {
  
});
