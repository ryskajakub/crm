define(['react'], function(React) {

  var originalData = [
    { author: "John Resig", comment: "JQuery rulez!!!" } 
    , { author: "Unknown", comment: "JQuery is mediocre!!!" }
  ];

  var Comment = React.createClass({
    render: function () {
      return (
        <div>
          <h2>{this.props.author}</h2>
          <span>{this.props.children}</span>
        </div>
      );
    }
  });
  var CommentList = React.createClass({
    render: function () {
      var nodes = this.props.data.map(function (comment) {
        return <Comment author={comment.author}>{comment.comment}</Comment>
      });
      return (
        <div>
          {nodes}
        </div>
      );
    }
  });
  var CommentBox = React.createClass({
    getInitialState: function () {
      return {data: originalData};
    } , 
    render: function () {
      return (
        <div>
          <CommentList data={this.state.data} />
          <CommentForm onCommentSubmit={this.handleCommentSubmit} />
        </div>
      );
    } ,
    handleCommentSubmit: function(comment) {
      console.log("comment"); 
      console.log(comment); 
      console.log("data"); 
      console.log(originalData); 
      var newData = originalData.push(comment);
      console.log("newData");
      console.log(originalData);
      var newState = {data: originalData};
      console.log(newState);
      this.setState(newState);
    }
  });
  var CommentForm = React.createClass({
    handleSubmit: function (e){
      e.preventDefault();
      var comment = this.refs.comment.getDOMNode().value.trim();
      if (comment) {
        this.props.onCommentSubmit({author: "Me", comment: comment});
        this.refs.comment.getDOMNode().value = "";
        return false;
      } else {
        return false;
      }
    } ,
    render: function() {
      return (
        <form onSubmit={this.handleSubmit}>
          <input type="text" ref="comment" />
          <input type="submit" />
        </form>
      );
    }
  });

  React.renderComponent(
    <CommentBox />
    , document.getElementById("js-app-container")
  )
  
});
