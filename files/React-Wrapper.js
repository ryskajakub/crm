var constructDOMElement = function(elementName, attributes, children, moreAttributes) {
  var obj = {};
  var addAttributes = function (attrs) {
    for (key in attrs) {
      if (key !== "instance") {
        var value = attrs[key];
        if (undefined === value["instance"]) {
          obj[key] = value;
        } else {
          if (value["instance"] === "Just") {
            var theValue = value["slot1"];
            obj[key] = theValue;
          }
        }
      }
    }
  }
  addAttributes(attributes);
  addAttributes(moreAttributes);
  return React.DOM[elementName](obj, children);
}
var declareReactClass = function(data) {
  return React.createClass({
    render: function() {
      return data.render(this);
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
