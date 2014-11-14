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
      return data.render([this.state, function(state) { t.setState(state);} ]);
    }
    , displayName: data.displayName
    , getInitialState: function() {
      return data.getInitialState
    }
    , componentDidMount: data.componentDidMount
  });
}
var renderReact = function(component) {
  React.renderComponent (
    component, document.getElementById('main')
  );
}
