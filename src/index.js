/*
  index.js
*/

"use strict";

var Main = require("./app/Main.purs");
Main.main();

// If hot-reloading, hook into each state change and re-render using the last
// state.
if (module.hot)
{
  module.hot.accept();
}
