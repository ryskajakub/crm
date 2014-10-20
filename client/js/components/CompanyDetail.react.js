/**
 * @jsx React.DOM
 */
var React = require('react');
var CompanyStore = require("../stores/CompanyStore");

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Jumbotron = B.Jumbotron;
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;
var Well = B.Well;

var Machine = React.createClass({
  render: function() {

    var type = this.props.type;
    var lastMaintenance = this.props.lastMaintenance;
    var imageSource = this.props.imageSource;

    return(
      <Col md={6}>
        <Well>
          <ListGroup>
            <ListGroupItem><strong>Typ</strong> {type}</ListGroupItem>
            <ListGroupItem><strong>Poslední servis</strong> {lastMaintenance}</ListGroupItem>
          </ListGroup>
          <img src={imageSource} />
        </Well>
      </Col>
    );
  }
});

var CompanyDetail = React.createClass({

  /**
   * @return {object}
   */
  render: function() {

    var company = this.state.company;

    return(
      <main>
        <section>
          <Jumbotron>
            <h1>{company.name}</h1>
            <ListGroup>
              <ListGroupItem>Brandýs nad labem</ListGroupItem>
              <ListGroupItem>p. Jelínek</ListGroupItem>
              <ListGroupItem>721 650 194</ListGroupItem>
            </ListGroup>
          </Jumbotron>
        </section>
        <section>
          <Grid>
            <Row className="show-grid">
              <Machine type="BK 15" imageSource="/images/remeza-bk15e.jpg" lastMaintenance="2.8.2014" />
              <Machine type="C-50.AB360" imageSource="/images/pistovy-kompresor-remeza-360-l-min-400-v.jpg" lastMaintenance="2.8.2013" />
            </Row>
          </Grid>
        </section>
      </main>
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
