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
var Well = B.Well;
var Input = B.Input;
var Button = B.Button;
var Glyphicon = B.Glyphicon;
var ButtonToolbar = B.ButtonToolbar;
var DropdownButton = B.DropdownButton;
var MenuItem = B.MenuItem;

var DocumentTitle = require('react-document-title');

var RecordMaintenance = React.createClass({

  render: function() {
    return (
      <Well>
        <h2>Zaznamenej servis</h2>
        <h3>ZICO</h3>
        <strong>Datum: </strong>5.11.2014
        <div>
          <strong>Technik: </strong>
          <ButtonToolbar>
            <DropdownButton title="vyber">
              <MenuItem>Kutička</MenuItem>
              <MenuItem>Mandlík</MenuItem>
            </DropdownButton>
          </ButtonToolbar>
        </div>
        <Input type="textarea" label="Poznámka" rows="5" />
        <Button bsStyle="primary"><Glyphicon glyph="ok" /> Zaznamenej servis</Button>
      </Well>
    );
  }

});

module.exports = RecordMaintenance;
