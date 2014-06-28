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

require(['react', 'jsx!bower_components/jsx-requirejs-plugin/js/components/Timer'], function(React, Timer) {
  var start = new Date();

  // Mount the JSX component in the app container
  React.renderComponent(
      Timer({start: start}),
      document.getElementById('js-app-container'));
});
