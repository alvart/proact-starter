/*
  karma.conf.js
*/

module.exports =
  function(config)
  {
    config.set
      (
        { autoWatch : true
        , browsers : [ "Chrome" ]
        , client : { mocha : { reporter : "html" } }
        , colors : true
        , files : [ "test/index.js" ]
        , frameworks : [ "mocha", "source-map-support" ]
        , logLevel : config.LOG_INFO
        , port : 9500
        , preprocessors : { "test/index.js" : [ "webpack", "sourcemap" ] }
        , singleRun : false
        ,
          webpack :
            { devtool : "inline-source-map"
            ,
              module :
              {
                loaders :
                  [
                    { test : /\.purs$/
                    , loader : "purs-loader"
                    , exclude : /node_modules/
                    ,
                      query :
                        { psc : "psa"
                        , pscArgs : { sourceMaps : true }
                        ,
                          src :
                            [ "bower_components/purescript-*/src/**/*.purs"
                            , "src/**/*.purs"
                            , "test/**/*.purs"
                            ]
                        , watch : true
                        }
                    }
                  ]
              }
            }
        }
      );
};
