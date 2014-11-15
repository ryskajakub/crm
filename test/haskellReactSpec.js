


describe("A suite", function() {
  it("contains spec with an expectation", function() {
    var singleElement = Fay$$_(HaskellReactSpec.singleElement,true);
    var ReactTestUtils = React.addons.TestUtils;
    var rendered = ReactTestUtils.renderIntoDocument(singleElement).getDOMNode();
    expect(rendered.className).toEqual("blue");
    expect(rendered.innerText).toEqual("Firma1");
  });
});
