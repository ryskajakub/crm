function listenToStoresMixin(store, resultName, fetch) {

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
}

module.exports = listenToStoresMixin;
