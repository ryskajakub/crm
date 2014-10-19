/**
 * @jsx React.DOM
 */
var React = require('react');
var B = require("react-bootstrap");
var LinkedStateMixin = require('react/lib/LinkedStateMixin');
var DocumentTitle = require('react-document-title');

var Grid = B.Grid;
var Row = B.Row;
var Col = B.Col;
var Input = B.Input;
var Button = B.Button;

var _ = require("underscore");

var CompanyActions = require("../actions/CompanyActions");
var CompanyFormStore = require("../stores/CompanyFormStore");

var CompanyNew = React.createClass({

  mixins: [LinkedStateMixin]

  , getInitialState: function() {
    return {
      "companyName": ""
      , "companyDays": ""
      , "companyNameError": ""
      , "companyNameAvailability" : {}
    };
  }

  , componentDidMount: function() {
    CompanyFormStore.addChangeListener(this.onNewState);
  }

  , componentWillUnmount: function() {
    CompanyFormStore.removeChangeListener(this.onNewState);
  }

  , onNewState: function () {
    this.setState(CompanyFormStore.get());
  }

  , displayLoading: function () {
    this.setState({"companyNameError": "Zkouším, jestli jméno firmy už není v programu."});
  }

  , checkNameAvailability: function () {
    this.displayLoading();
    var enteredName = event.target.value;
    this.setState({"companyName": enteredName});
    CompanyActions.checkNameAvailability(enteredName);
  }

  , isInputSameAsResponse: function (input, response) {
    var values = _.values(response);
    if (_.size(values) == 0) {
      return false;
    } else {
      var responseValue = values[0];
      var inputValue = input;
      return (inputValue === responseValue);
    }
  }

  /**
   * @return {object}
   */
  , render: function() {

    var currentTextInCompanyNameInput = this.state.companyName;
    var availabilityResponse = this.state.companyNameAvailability;
    var companyNameHelp = 
      (this.isInputSameAsResponse(currentTextInCompanyNameInput, availabilityResponse))
      ? ("fail" === _.keys(availabilityResponse)[0] ? "Jméno již je v programu" : "OK")
      : this.state.companyNameError;

    return(
      <DocumentTitle title={"CRM - Nová firma"}>
        <Grid>
          <Row>
            <Col lg={6} lgOffset={3}>
              <Input type="text" label="Jméno firmy" onChange={this.checkNameAvailability}
                help={companyNameHelp} />
              <Input type="text" label="Dny" valueLink={this.linkState("companyDays")} />
              <Button bsStyle="primary" bsSize="large" onClick={this.click}>Vytvořit</Button>
            </Col>
          </Row>
        </Grid>
      </DocumentTitle>
    );
  }

  , click: function() {
    var company = {
      "name": this.state.companyName
      , "days": parseInt(this.state.companyDays)
    }

    CompanyActions.createCompany(company);

  }

});

module.exports = CompanyNew;
