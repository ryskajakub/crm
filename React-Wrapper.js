var declareReactClass = function(header) {
  return React.createClass({
    render: function() {
      return (
        React.DOM[header](this.props, header)
      );
    }
  });
};
var renderReact = function(component) {
  React.renderComponent (
    component, document.getElementById('main')
  );
}
