var constructDOMElement = function(elementName, attributes, children) {
  console.log(attributes);
  return React.DOM[elementName](attributes, children);
}
var declareReactClass = function(data) {

  return React.createClass({
    render: function() {
      return data.render([this.state, this.props]);
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
