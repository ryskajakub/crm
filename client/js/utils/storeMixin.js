var storeMixin = {
  get: function(id) {
    return (
      (id === undefined) ?
      this.getCollection() :
      this.getCollection()[id]
    );
  }
}

module.exports = storeMixin;
