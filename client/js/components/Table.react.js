/**
 * @jsx React.DOM
 */

var React = require("react");
var _ = require("underscore");
var CompanyRow = require("./CompanyRow.react");
var CompanyStore = require("../stores/CompanyStore");
var Moment = require("../utils/Moment");

var B = require("react-bootstrap");
var BTable = B.Table;
var Button = B.Button;

var DocumentTitle = require('react-document-title');

var Table = React.createClass({

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
          <BTable striped bordered>
            <thead>
              <tr><th>Název</th><th>Servis za</th></tr>
            </thead>
            <tbody>
              {rowsHtml}
            </tbody>
          </BTable>
        </main>
      </DocumentTitle>
    );
  }

});

module.exports = Table;
