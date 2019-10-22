var path = require("path");

module.exports = {
  mode: 'none',
  entry: {
    app: [
    './src/index.js'
    ]
  },
  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: 'main.js',
  },
  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: [ "style-loader","css-loader" ]
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file?name=[name].[ext]',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader',
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      },
    ],
    noParse: /\.elm$/,
  },
  devServer: {
    inline: true,
    stats: { colors: true },
  },
};