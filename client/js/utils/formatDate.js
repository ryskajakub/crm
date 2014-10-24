var formatDate = function(dateObject) {
  return(
    "Day" === dateObject["accuracy"]
    ? dateObject["date"].format("D.MMMM YYYY (dddd)")
    : dateObject["date"].format("MMMM YYYY")
  );
}

module.exports = formatDate;
