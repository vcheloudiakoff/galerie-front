module.exports = {
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {
          pathToElm: 'node_modules/.bin/elm'
        }
      }
    },
    {test: /\.css$/, use: ['style-loader', 'css-loader']}]
  }
};
