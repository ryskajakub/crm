/**
 * @jsx React.DOM
 */

var React = require("react");
var _ = require("underscore");
var CompanyRow = require("./CompanyRow.react");
var CompanyStore = require("../stores/CompanyStore");
var Moment = require("../utils/Moment");
var Router = require('react-router');

var B = require("react-bootstrap");
var Table = B.Table;
var Button = B.Button;
var Glyphicon = B.Glyphicon;

var DocumentTitle = require('react-document-title');

var Router = require('react-router');
var Navigation = Router.Navigation;

var CompaniesList = React.createClass({

  mixins: [Navigation]

  , goToNewCompany: function() {
    this.transitionTo("company-new");
  }, 

  getInitialState: function() {
    return {
      "rows": CompanyStore.get()
      , "showInactive": true
    }
  },

  componentDidMount: function() {
    CompanyStore.addChangeListener(this.onNewState);
  },

  componentWillUnmount: function() {
    CompanyStore.removeChangeListener(this.onNewState);
  },

  onNewState: function () {
    this.setState({"rows": CompanyStore.get()});
  },

  showInactive: function () {
    this.setState({"showInactive": !this.state.showInactive});
  }

  /**
   * @return {object}
   */
  , render: function() {

    var rows = this.state.rows;
    var activeText = this.state.showInactive ? "Skrýt neaktivní" : "Ukázat neaktivní";

    var rowsFilteredByActive =
      (this.state.showInactive)
      ? rows
      : _.filter(rows, function (row) {
        return (row.active);
      });

    var rowsHtml = _.reduce(rowsFilteredByActive, function(acc, value, key) {
      var elem =
        <CompanyRow companyRow={value} key={key} />
      acc.push(elem);
      return acc;
    }, []);

    return (
      <DocumentTitle title={"CRM - Seznam firem"}>
        <main>
          <section>
            <Button onClick={this.goToNewCompany}>
              <Glyphicon glyph="plus" /> Přidat firmu
            </Button>
          </section>
          <section>
            <Table striped bordered>
              <thead>
                <tr><th>Název firmy</th><th>Platnost servisu vyprší za</th></tr>
              </thead>
              <tbody>
                {rowsHtml}
              </tbody>
            </Table>
          </section>
        </main>
      </DocumentTitle>
    );
  }

});

module.exports = CompaniesList;
