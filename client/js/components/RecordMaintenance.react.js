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

var RecordMaintenance = React.createClass({

  render: function() {
    return (
      <div>Zapiš servis</div>
    );
  }

});

module.exports = RecordMaintenance;
