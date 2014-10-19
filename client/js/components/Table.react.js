/**
 * @jsx React.DOM
 */

var React = require("react");
var _ = require("underscore");
var CompanyRow = require("./CompanyRow.react");
var CompanyStore = require("../stores/CompanyStore");

var B = require("react-bootstrap");
var BTable = B.Table;
var Button = B.Button;

var DocumentTitle = require('react-document-title');

var Table = React.createClass({

  getInitialState: function() {
    return {
      "rows": CompanyStore.get()
      , "showActive": false
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

  showActive: function () {
    this.setState({"showActive": !this.state.showActive});
  }

  /**
   * @return {object}
   */
  , render: function() {

    var rows = this.state.rows;
    var activeText = this.state.showActive ? "Skrýt aktivní" : "Ukázat aktivní";

    var rowsHtml = _.reduce(rows, function(acc, value, key) {
      var elem = 
        <CompanyRow companyRow={value} key={key} />
      acc.push(elem);
      return acc;
    }, []);


    return (
      <DocumentTitle title={"CRM - Seznam firem"}>
        <main>
          <Button bsStyle="primary" onClick={this.showActive}>{activeText}</Button>
          <BTable striped bordered>
            <thead>
              <tr><th>Název</th><th>Dny</th></tr>
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
