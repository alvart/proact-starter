/*
  index.js
*/
"use strict";

var Adapter = require("enzyme-adapter-react-16");
var Enzyme = require("enzyme");
var Main = require("./app/Main.purs");

Enzyme.configure({ adapter : new Adapter() });

Main.main();
