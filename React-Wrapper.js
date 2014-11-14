var constructDOMElement = function(elementName, attributes, children) {
  return React.DOM[elementName](attributes, children);
}
var declareReactClass = function(r) {
  return React.createClass({
    render: r.render
    , displayName: r.displayName
  });
}
var renderReact = function(component) {
  React.renderComponent (
    component, document.getElementById('main')
  );
}
