/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");
var Router = require('react-router');

var CompanyStore = require("../stores/CompanyStore");
var MachineStore = require("../stores/MachineStore");
var BigMachine = require("./BigMachine.react");
var EditableField = require("./EditableField.react");

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Jumbotron = B.Jumbotron;
var Glyphicon = B.Glyphicon;
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

    var editing = (this.props.name == "company-edit") ? true : false;

    var machinesTags = _.reduce(machinesInCompany, function(acc, value, key) {
      var machine = (<BigMachine key={key} type={value.type} image={value.image} maintenanceDate={value.lastMaintenance} />);
      acc.push(machine);
      return acc;
    }, []);

    var editAction = (editing)
      ? <Link to='company-detail' params={{"companyId": id}}><Glyphicon glyph="ok" className="goRight" /></Link>
      : <Link to='company-edit' params={{"companyId": id}}><Glyphicon glyph="pencil" className="goRight" /></Link> ;

    return(
      <main>
        <section>
          <Jumbotron>
            <h1>{company.name} - {company.plant} {editAction}</h1>
            <dl className="dl-horizontal">
              <dt>Adresa</dt>
              <dd><EditableField initialValue={company.address} editing={editing} /></dd>
              <dt>Kontakt</dt>
              <dd><EditableField initialValue={company.contact} editing={editing} /></dd>
              <dt>Telefon</dt>
              <dd><EditableField initialValue={company.phone} editing={editing} /></dd>
            </dl>
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
