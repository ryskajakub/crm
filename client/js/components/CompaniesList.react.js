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
var listenToStoreSimpleMixin = require("../utils/listenToStoreSimpleMixin");

var CompaniesList = React.createClass({

  mixins: [Navigation, listenToStoreSimpleMixin(CompanyStore, "companies", function(component, store) {
    return store.get();
  })]

  , goToNewCompany: function() {
    this.transitionTo("company-new");
  }

  , render: function() {

    var rows = this.state.companies;

    var rowsHtml = _.reduce(rows, function(acc, value, key) {
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
