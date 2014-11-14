var constructDOMElement = function(elementName, attributes, children) {

  console.log(attributes);

  return React.DOM[elementName]({
    className: attributes.className
    , onClick: function () { 
      try {
        attributes.onClick()() 
      } catch (err) {
      }
    }
  }, children);
}
var declareReactClass = function(data) {
  return React.createClass({
    render: function() {
      return data.render(this.state);
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
