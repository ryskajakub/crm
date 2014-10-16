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

var CompanyActions = require("../actions/CompanyActions")

var CompanyNew = React.createClass({

	mixins: [LinkedStateMixin]

	, getInitialState: function() {
		return {
			"companyName": ""
			, "companyDays": ""
		};
	}

  /**
   * @return {object}
   */
  , render: function() {
		return(
			<DocumentTitle title={"CRM - Nová firma"}>
				<Grid>
					<Row>
						<Col lg={6} lgOffset={3}>
							<Input type="text" label="Jméno firmy" valueLink={this.linkState("companyName")} />
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
