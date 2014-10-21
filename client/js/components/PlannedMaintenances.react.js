/**
 * @jsx React.DOM
 */
var React = require('react');

var _ = require("underscore");
var Moment = require("../utils/Moment");

var Router = require('react-router');
var B = require("react-bootstrap");

var Table = B.Table;
var Link = Router.Link;

var DocumentTitle = require('react-document-title');

var PlannedMaintenances = React.createClass({

  render: function() {

    var planned = [
      ["CAHA", "1.11.2014"]
      , ["ZICO", "5.11.2014"]
      , ["VTP - trigema", "10.11.2014"]
    ];

    var rows = _.reduce(planned, function(acc, elem) {
      var row = 
        <tr>
          <td>
            <Link to='maintenance'>{elem[0]}</Link>
          </td>
          <td>
            {elem[1]}
          </td>
        </tr>;
      acc.push(row);
      return acc;
    }, []);

    return (
      <DocumentTitle title={"CRM - Naplánované servisy"}>
        <main>
          <Table striped bordered>
            <thead>
              <tr><th>Název firmy</th><th>Servis naplánován</th></tr>
            </thead>
            <tbody>
              {rows}
            </tbody>
          </Table>
        </main>
      </DocumentTitle>
    );
  }

});

module.exports = PlannedMaintenances
