var _ = require("underscore");

function listenToStoresMixin(store, resultName, fetch) {

  var numArguments =  _.size(arguments);

  if (numArguments === 3) {
    var StoreMixin = {
      getInitialState: function() {
        var result = {}
        result[resultName] = fetch(this, store);
        return result;
      }

      , componentDidMount: function() {
        store.addChangeListener(this.handleStoresChanged);
        this.setState(this.getInitialState());
      }

      , componentWillUnmount: function() {
        store.removeChangeListener(this.handleStoresChanged);
      }

      , handleStoresChanged: function() {
        if (this.isMounted()) {
          this.setState(getInitialState());
        }
      }
    };
    return StoreMixin;
  } else if (numArguments === 1) {

    // stores in array
    var stores = arguments[0];

    // depends on computeStateFromStores()
    var StoreMixin = {
      getInitialState: function() {
        return this.computeStateFromStores();
      }

      , handleStoresChanged: function() {
        if (this.isMounted()) {
          this.setState(this.computeStateFromStores());
        }
      }

      , componentDidMount: function() {
        var t = this;
        _.forEach(stores, function(store) {
          store.addChangeListener(t.handleStoresChanged);
        });
        this.setState(this.computeStateFromStores());
      }

      , componentWillUnmount: function() {
        var t = this;
        _.forEach(stores, function(store) {
          store.removeChangeListener(t.handleStoresChanged);
        });
      }
    };
    return StoreMixin;
  }
}

module.exports = listenToStoresMixin;
