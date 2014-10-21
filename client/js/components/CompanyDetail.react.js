/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");
var Router = require('react-router');

var CompanyStore = require("../stores/CompanyStore");
var MachineStore = require("../stores/MachineStore");
var BigMachine = require("./BigMachine.react");

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Jumbotron = B.Jumbotron;
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;
var Well = B.Well;
var Input = B.Input;
var Button = B.Button;
var Panel = B.Panel;
var Link = Router.Link;

var CompanyDetail = React.createClass({

  /**
   * @return {object}
   */
  render: function() {

    var company = this.state.company;
    var machinesInCompany = this.state.machines;
    var id = this.props.params.companyId;

    var machinesTags = _.reduce(machinesInCompany, function(acc, value, key) {
      var machine = (<BigMachine key={key} type={value.type} image={value.image} maintenanceDate={value.lastMaintenance} />);
      acc.push(machine);
      return acc;
    }, []);

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
            <Row>
              {machinesTags}
            </Row>
            <Row>
              <Col md={12}>
                <Panel>
                  <Link to='maintenance' params={{companyId: id, maintenanceId: "new"}}> 
                    Naplánovat servis
                  </Link>
                </Panel>
              </Col>
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
