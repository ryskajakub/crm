var constructDOMElement = function(elementName, children) {
  return React.DOM[elementName](null, children);
}
var declareReactClass = function(element) {
  return React.createClass({
    render: function() {
      return (
        element
      );
    }
  });
}
var renderReact = function(component) {
  React.renderComponent (
    component, document.getElementById('main')
  );
}
