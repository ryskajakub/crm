/**
 * @jsx React.DOM
 */
var React = require('react');
var CompanyStore = require("../stores/CompanyStore");

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Jumbotron = B.Jumbotron;

var CompanyDetail = React.createClass({

  /**
   * @return {object}
   */
  render: function() {

    var company = this.state.company;

    return(
      <Jumbotron>
        <section>
          <h1>{company.name}</h1>
          <ListGroup>
            <ListGroupItem>Brandýs nad labem</ListGroupItem>
            <ListGroupItem>p. Jelínek</ListGroupItem>
            <ListGroupItem>721 650 194</ListGroupItem>
          </ListGroup>
        </section>
      </Jumbotron>
    );
  }

  , getInitialState: function () {
    return this.getCompanyById(this.props.params.companyId);
  }

  , getCompanyById: function(id) {
    var company = CompanyStore.get(id);
    return {
      "company": company
    };
  }

});

module.exports = CompanyDetail;
