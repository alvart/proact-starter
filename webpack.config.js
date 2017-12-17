/*
  webpack.config.js
*/

"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const Path = require("path");
const UglifyJsPlugin = require("webpack/lib/optimize/UglifyJsPlugin");
const Webpack = require("webpack");

const isProduction = process.env.NODE_ENV === "production";

module.exports =
  {
    devServer :
      { compress : isProduction
      , contentBase : Path.join(__dirname, "/src/")
      , historyApiFallback : true
      , host : "localhost"
      , inline : true
      , port : 3000
      , stats : "minimal"
      , watchOptions : { aggregateTimeout : 300, poll : 1000 }
      }
  , entry : [ Path.join(__dirname, "src/index.js") ]
  ,
    module :
      {
        rules :
          [
            { test : /\.purs$/
            , loader : "purs-loader"
            , exclude : /node_modules/
            ,
              query :
                { bundle : isProduction
                , psc : "psa"

                // Uglified purescript source-map is broken.
                // Disabling it for production.
                , pscArgs : { sourceMaps : !isProduction }
                ,
                  src :
                    [ "bower_components/purescript-*/src/**/*.purs"
                    , "src/**/*.purs"
                    ]
                , watch : !isProduction
                }
            }
          ]
      }
  ,
    output :
      { path : Path.join(__dirname, "/dist/")
      , publicPath : "/"
      , ...
        isProduction
          ? { filename : "[name]-[hash].min.js" }
          : { filename : "[name].js", pathinfo : true }
      }
  ,
    plugins :
      [
        new HtmlWebpackPlugin
          (
            { template : "src/index.html"
            , inject : "body"
            , filename : "index.html"
            }
          )
      , new Webpack.ProgressPlugin()
      , new Webpack.SourceMapDevToolPlugin({ filename : "[file].map" })
      , ...
        isProduction
          ?
            [ new UglifyJsPlugin({ beautify : false, comments : false })
            ,
              new Webpack.LoaderOptionsPlugin
                ({ minimize : true, debug : false })
            ]
          : []
      ]
  , resolve : { modules : [ "node_modules", "output" ] }

  // Uglified purescript source-map is broken. Disabling it for production.
  , ...
    isProduction
      ? { }
      : { devtool : "eval-source-map" }
  };
