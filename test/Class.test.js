
describe("A suite", function() {
  it("contains spec with an expectation", function() {
    var render = Fay$$_(Class.singleElement,true);
    var ReactTestUtils = React.addons.TestUtils;
    var instance = ReactTestUtils.renderIntoDocument(render);
    expect(instance.getDOMNode().className === "blue").toBe(true);
  });
});
