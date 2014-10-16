/**
 * @jsx React.DOM
 */
var React = require('react');
var B = require("react-bootstrap");

var Grid = B.Grid;
var Row = B.Row;
var Col = B.Col;
var Input = B.Input;
var Button = B.Button;

var CompanyNew = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
		return(
			<Grid>
				<Row>
					<Col lg={6} lgOffset={3}>
						<Input type="text" label="Jméno firmy" />
						<Input type="text" label="Dny" />
						<Button bsStyle="primary" bsSize="large">Vytvořit</Button>
					</Col>
				</Row>
			</Grid>
		);
	}

});

module.exports = CompanyNew;
