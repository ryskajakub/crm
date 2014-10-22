function togglableStateMixin() {

  var TogglableStateMixin = {
    getInitialState: function() {
      return {"active": false};
    }

    , toggle: function () {
      console.log("toggle");
      this.setState({"active": !this.state.active});
    }

  };

  return TogglableStateMixin;
}

module.exports = togglableStateMixin;
