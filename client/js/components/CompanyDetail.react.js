/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");
var Router = require('react-router');
var LinkedStateMixin = require('react/lib/LinkedStateMixin');

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

  mixins: [LinkedStateMixin]

  , saveEdit: function() {
    console.log("saving edit!");
    console.log(this.state);
  }

  /**
   * @return {object}
   */
  , render: function() {

    var company = this.state.company;
    var machinesInCompany = this.state.machines;
    var id = this.props.params.companyId;

    var editing = (this.props.name == "company-edit" || this.props.name === "company-new") ? true : false;

    var machinesTags = _.reduce(machinesInCompany, function(acc, value, key) {
      var machine = (<BigMachine key={key} type={value.type} image={value.image} maintenanceDate={value.lastMaintenance} />);
      acc.push(machine);
      return acc;
    }, []);

    var editAction = (editing)
      ? <Glyphicon glyph="ok" className="goRight" onClick={this.saveEdit} />
      : <Link to='company-edit' params={{"companyId": id}}><Glyphicon glyph="pencil" className="goRight" /></Link> ;

    var setValue = function(field) {
      return function(value) {
        console.log(value);
      }
    }

    return(
      <main>
        <section>
          <Jumbotron>
            <h1>
              <EditableField setValue={setValue("name")} initialValue={company.name} editing={editing} 
                groupClassName="inline" label="Jméno firmy" />
              -
              <EditableField setValue={setValue("plant")} initialValue={company.plant} editing={editing} 
                groupClassName="inline" label="Označení pobočky" />
              {editAction}
            </h1>
            <dl className="dl-horizontal">
              <dt>Adresa</dt>
              <dd><EditableField setValue={setValue("address")} initialValue={company.address} editing={editing} /></dd>
              <dt>Kontakt</dt>
              <dd><EditableField setValue={setValue("contact")} initialValue={company.contact} editing={editing} /></dd>
              <dt>Telefon</dt>
              <dd><EditableField setValue={setValue("phone")} initialValue={company.phone} editing={editing} /></dd>
            </dl>
          </Jumbotron>
        </section>
        {
          ("company-new" === this.props.name) ? "" :
            <section>
              <Grid>
                <Row>
                  {machinesTags}
                  <Col md={4}>
                    <Panel>
                      <h2><Link to='machine-detail' params={{companyId: id, machineId: "new"}}>Nový stroj</Link></h2>
                    </Panel>
                  </Col>
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
        }
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
