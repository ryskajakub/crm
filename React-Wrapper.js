var constructDOMElement = function(elementName, attributes, children) {
  return React.DOM[elementName]({
    className: attributes.className
    , onClick: attributes.onClick
  }, children);
}
var declareReactClass = function(data) {
  return React.createClass({
    render: function() {
      var t = this;
      return data.render([this.state, function(state) { 
        t.setState(state);
      } ]);
    }
    , displayName: data.displayName
    , getInitialState: function() {
      var initialState = Fay$$_(data.getInitialState);
      var object = {};
      for (var key in initialState) {
        if ("instance" !== key) {
          object[key] = Fay$$_(initialState[key]);
        }
      }
      return object;
    }
    , componentDidMount: data.componentDidMount
  });
}
var renderReact = function(component) {
  React.renderComponent (
    component, document.getElementById('main')
  );
}
