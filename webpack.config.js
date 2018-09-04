'use strict';

const path = require('path');

const webpack = require('webpack');

const isWebpackDevServer = process.argv.filter(a => path.basename(a) === 'webpack-dev-server').length;

const isWatch = process.argv.filter(a => a === '--watch').length

const plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

console.log('DIRNAME', __dirname);

module.exports = {
  // devtool: 'eval-source-map',

  devServer: {
    contentBase: '.',
    port: 4008/*,
    stats: 'errors-only'*/
  },

  entry: './example/Main.purs',

  output: {
    path: path.resolve(__dirname, 'output'),
    publicPath: '/output/',
    pathinfo: true,
    filename: 'app.js'
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              exclude: /output/,
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'src/**/*.purs',
                'example/**/*.purs'
              ],
              // bundle: true,
              // pscBundleArgs: {
              //   main: "Main"
              // },
              psc: 'psa',
              watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      },
    ]
  },

  resolve: {
    modules: [ 'node_modules', 'bower_components' ],
    //root: [ path.resolve('./src') ],
    extensions: [ '.purs', '.js']
  },

  // devServer : {
  //   contentBase : './output'
  // },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ].concat(plugins)
};
