module.exports = {
  entry: "./js/crm.js",
  output: {
    path: "js"
    , filename: "bundle.js"
  } ,
  module: {
    loaders: [
      { test: /\.js$/, loader: "jsx-loader" }
    ]
  }
};
