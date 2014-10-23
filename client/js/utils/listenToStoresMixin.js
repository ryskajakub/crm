var _ = require("underscore");

function listenToStoresMixin(stores) {

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

module.exports = listenToStoresMixin;
