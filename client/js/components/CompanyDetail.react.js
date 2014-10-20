/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");

var CompanyStore = require("../stores/CompanyStore");
var MachineStore = require("../stores/MachineStore");

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
    var machinesInCompany = this.state.machines;

    var array = [];
    _.forEach(machinesInCompany, function(value, key) {
      var machine = (<Machine kep={key} type={value.type} imageSource={value.image} lastMaintenance={value.lastMaintenance} />);
      array.push(machine);
    })

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
              {array}
            </Row>
          </Grid>
        </section>
      </main>
    );
  }

  , componentDidMount: function() {
    CompanyStore.addChangeListener(this.onNewState);
    MachineStore.addChangeListener(this.onNewState);
  }

  , componentWillUnmount: function() {
    CompanyStore.removeChangeListener(this.onNewState);
    MachineStore.removeChangeListener(this.onNewState);
  }

  , onNewState: function () {
    this.setState(this.getInitialState());
  }

  , getInitialState: function () {
    return this.getCompanyById(this.props.params.companyId);
  }

  , getCompanyById: function(id) {
    var company = CompanyStore.get(id);
    var machines = MachineStore.getByCompanyId(id);
    return {
      "company": company
      , "machines": machines
    };
  }

});

module.exports = CompanyDetail;
