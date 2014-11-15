
describe("A suite", function() {
  it("contains spec with an expectation", function() {
    var singleElement = Fay$$_(HaskellReactSpec.singleElement, true);
    var ReactTestUtils = React.addons.TestUtils;
    var rendered = ReactTestUtils.renderIntoDocument(singleElement).getDOMNode();
    expect(rendered.innerText).toEqual("The header 0");
    ReactTestUtils.Simulate.click(rendered);
    ReactTestUtils.Simulate.click(rendered);
    expect(rendered.innerText).toEqual("The header 2");
  });
});
