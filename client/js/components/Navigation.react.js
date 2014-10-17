/**
 * @jsx React.DOM
 */

var React = require("react");
var CompanyActions = require("../actions/CompanyActions")

var Router = require('react-router');
var Link = Router.Link;

var B = require("react-bootstrap"); 
var Navbar = B.Navbar;
var Nav = B.Nav;
var NavItem = B.NavItem;

var NavLink = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
    return (
      <li>
        <Link to={this.props.to}>{this.props.children}</Link>
      </li>
    );
  }

});

var Navigation = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
    return (
      <div>
        <Navbar>
          <Nav>
            <NavLink to='table'>Hlavní strana</NavLink>
            <NavLink to='company-new'>Nová firma</NavLink>
          </Nav>
        </Navbar>
        <this.props.activeRouteHandler />
      </div>
    );
  }

});

module.exports = Navigation;
